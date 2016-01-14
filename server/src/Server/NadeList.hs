{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.NadeList where

import API.Nades
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Int
import qualified Data.Function as F
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Database.Nades as DN
import Database.Esqueleto
import Servant
import Servant.Server

import API.SignIn
import API.Nades
import Server.App
import Server.Nade
import Server.SignIn

nadeListFromDb :: Entity DN.NadeList -> [Entity DN.Nade] -> NadeList
nadeListFromDb nadeList nades =
  nadeListFromDb' key (entityVal nadeList) (fmap nadeFromDb nades)
  where key = fromSqlKey . entityKey $ nadeList

nadeListFromDb' :: Int64 -> DN.NadeList -> [Nade] -> NadeList
nadeListFromDb' key DN.NadeList{..} nades =
  dbFill nadeListAuthorId key $ NadeList'' nadeListTitle nadeListDescription nades

nadeListToDb' :: Text -> NadeList' -> DN.NadeList
nadeListToDb' author NadeList''{..} =
  DN.NadeList author _nadeListTitle _nadeListDescription

nadeListingsForDb :: Key DN.NadeList -> NadeList' -> [DN.NadeListing]
nadeListingsForDb nadeListKey NadeList''{..} =
  fmap f . zip [1..] $ _nadeListNades
  where f (ord, nadeKey) = DN.NadeListing ord (toSqlKey nadeKey) nadeListKey

collapseNadeListings :: (Eq a) => [(a, b)] -> [(a, [b])]
collapseNadeListings = fmap f . L.groupBy ((==) `F.on` fst)
  where f nades = (fst . head $ nades, fmap snd nades)

nadeListServer :: ServerT NadeListAPI App
nadeListServer =
  getNadeLists
  :<|> postNadeList :<|> getNadeList :<|> putNadeList :<|> deleteNadeList
  :<|> withCookieText myNadeLists

type FullNadeListFilter = SqlExpr (Entity DN.Nade) -> SqlExpr (Entity DN.NadeListing) -> SqlExpr (Entity DN.NadeList) -> SqlQuery ()

noFNLFilter :: FullNadeListFilter
noFNLFilter _ _ _ = return ()

authorFNLFilter :: Text -> FullNadeListFilter
authorFNLFilter author n nlg nl =
  where_ (nl ^. DN.NadeListAuthorId ==. val author)

keyFNLFilter :: Key DN.NadeList -> FullNadeListFilter
keyFNLFilter key n nlg nl =
  where_ (nl ^. DN.NadeListId ==. val key)

authorKeyFNLFilter :: Text -> Key DN.NadeList -> FullNadeListFilter
authorKeyFNLFilter author key n nlg nl =
  where_ (nl ^. DN.NadeListId ==. val key
          &&. nl ^. DN.NadeListAuthorId ==. val author)

-- TODO: LeftOuterJoin to include empty NadeLists
fnlsQuery' :: FullNadeListFilter -> SqlQuery (SqlExpr (Entity DN.NadeList), SqlExpr (Entity DN.Nade))
fnlsQuery' filter = from $ \(n `InnerJoin` nlg `InnerJoin` nl) -> do
  on (nl ^. DN.NadeListId ==. nlg ^. DN.NadeListingNadeList)
  on (n ^. DN.NadeId ==. nlg ^. DN.NadeListingNade)
  filter n nlg nl
  orderBy [asc (nl ^. DN.NadeListId), asc (nlg ^. DN.NadeListingOrdinal)]
  return (nl, n)

fnlsQuery :: (MonadIO m) => FullNadeListFilter -> SqlPersistT m [NadeList]
fnlsQuery filter = do
  nadeListings <- select $ fnlsQuery' filter
  return . fmap (uncurry nadeListFromDb) . collapseNadeListings $ nadeListings

filteredFNLs :: FullNadeListFilter -> App [NadeList]
filteredFNLs filter = runDb $ fnlsQuery filter

getNadeLists :: App [NadeList]
getNadeLists = filteredFNLs noFNLFilter

myNadeLists :: CookieData -> App [NadeList]
myNadeLists CookieData{..} =
  filteredFNLs $ authorFNLFilter _cookieDataUserId

getNadeList :: Int64 -> App NadeList
getNadeList key =
  firstOr404 =<< filteredFNLs (keyFNLFilter $ toSqlKey key)

insertNadeListings' :: (MonadIO m) => Key DN.NadeList -> NadeList' -> SqlPersistT m [Key DN.NadeListing]
insertNadeListings' nadeListKey nadeList =
  insertMany . nadeListingsForDb nadeListKey $ nadeList

insertNadeList' :: (MonadIO m) => Text -> NadeList' -> SqlPersistT m (Key DN.NadeList, [Key DN.NadeListing])
insertNadeList' author nadeList = do
  nadeListKey <- insert . nadeListToDb' author $ nadeList
  nadeListingKeys <- insertNadeListings' nadeListKey nadeList
  return (nadeListKey, nadeListingKeys)

insertNadeList :: (MonadIO m) => Text -> NadeList' -> SqlPersistT m (Maybe NadeList)
insertNadeList author nadeList = do
  (nadeListKey, _) <- insertNadeList' author nadeList
  nadeLists <- fnlsQuery $ keyFNLFilter nadeListKey
  return $ listToMaybe nadeLists

postNadeList :: NadeList' -> Maybe Text -> App NadeList
postNadeList nadeList =
  withCookieText
  (\(CookieData{..}) -> do
      nadeList' <- runDb $ insertNadeList _cookieDataUserId nadeList
      maybe (throwWrapped err500) return nadeList'
  )

type NadeListingFilter = SqlExpr (Entity DN.NadeListing) -> SqlQuery ()

nlNLGFilter :: Key DN.NadeList -> NadeListingFilter
nlNLGFilter nadeListKey nlg =
  where_ (nlg ^. DN.NadeListingNadeList ==. val nadeListKey)

setNadeList' :: NadeList' -> SqlExpr (Entity DN.NadeList) -> SqlQuery ()
setNadeList' NadeList''{..} nl =
  set nl [ DN.NadeListTitle =. val _nadeListTitle
         , DN.NadeListDescription =. val _nadeListDescription
         ]

type NadeListFilter = SqlExpr (Entity DN.NadeList) -> SqlQuery ()

authorKeyNLFilter :: Text -> Key DN.NadeList -> NadeListFilter
authorKeyNLFilter author key nl =
  where_ (nl ^. DN.NadeListId ==. val key
          &&. nl ^. DN.NadeListAuthorId ==. val author)

updateNadeList' :: (MonadIO m) => Text -> Key DN.NadeList -> NadeList' -> SqlPersistT m Int64
updateNadeList' author key nadeList =
  updateCount $ (\nl -> do
               setNadeList' nadeList nl
               authorKeyNLFilter author key nl
           )

deleteNadeListings :: (MonadIO m) => Key DN.NadeList -> SqlPersistT m ()
deleteNadeListings key =
  delete $ from $ nlNLGFilter key

putNadeList :: Int64 -> NadeList' -> Maybe Text -> App NadeList'
putNadeList key nadeList =
  withCookieText
  (\(CookieData{..}) -> do
      notAuthor <- runDb $ do
        let key' = toSqlKey key
        updated <- updateNadeList' _cookieDataUserId key' nadeList
        if (updated <= 0)
          then return True
          else do
          deleteNadeListings key'
          insertNadeListings' key' nadeList
          return False
      when notAuthor $ throwWrapped err403
      return nadeList
  )

deleteNadeList :: Int64 -> Maybe Text -> App ()
deleteNadeList key =
  withCookieText
  (\(CookieData{..}) -> do
      notAuthor <- runDb $ do
        let key' = toSqlKey key
        deleted <- deleteCount $ from $ authorKeyNLFilter _cookieDataUserId key'
        if (deleted > 0)
          then do
          delete $ from $ nlNLGFilter key'
          return False
          else return True
      when notAuthor $ throwWrapped err403
  )

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Nades where

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
import Server.App
import Server.SignIn

nadeFromDb :: Entity DN.Nade -> Nade
nadeFromDb nade = nadeFromDb' (fromSqlKey . entityKey $ nade) (entityVal nade)

nadeFromDb' :: Int64 -> DN.Nade -> Nade
nadeFromDb' key DN.Nade{..} =
  dbFill nadeAuthorId key $ Nade' nadeImages nadeTitle nadeDescription nadeTags

nadeListFromDb :: Entity DN.NadeList -> [Entity DN.Nade] -> NadeList
nadeListFromDb nadeList nades =
  nadeListFromDb' key (entityVal nadeList) (fmap nadeFromDb nades)
  where key = fromSqlKey . entityKey $ nadeList

nadeListFromDb' :: Int64 -> DN.NadeList -> [Nade] -> NadeList
nadeListFromDb' key DN.NadeList{..} nades =
  dbFill nadeListAuthorId key $ NadeList'' nadeListTitle nadeListDescription nades

nadeToDb :: Nade -> DN.Nade
nadeToDb nade = nadeToDb' (_dbFilledAuthor nade) (_dbFilledContents nade)

nadeToDb' :: Text -> Nade' -> DN.Nade
nadeToDb' author Nade'{..} =
  DN.Nade author _nadeImages _nadeTitle _nadeDescription _nadeTags

nadeListToDb' :: Text -> NadeList' -> DN.NadeList
nadeListToDb' author NadeList''{..} =
  DN.NadeList author _nadeListTitle _nadeListDescription

nadeServer :: ServerT NadeAPI App
nadeServer =
  getNades
  :<|> postNade :<|> getNade :<|> putNade :<|> deleteNade
  :<|> withCookieText myNades
  :<|> getNadeLists
  :<|> postNadeList :<|> getNadeList :<|> putNadeList :<|> deleteNadeList
  :<|> withCookieText myNadeLists

getNades :: App [Nade]
getNades = do
  nades <- runDb $ select $ from $ return
  let nades' = fmap nadeFromDb nades
  return nades'

myNades :: CookieData -> App [Nade]
myNades CookieData{..} = do
  nades <- runDb $ select $ from $ \n -> do
    where_ (n ^. DN.NadeAuthorId ==. val _cookieDataUserId)
    return n
  let nades' = fmap nadeFromDb nades
  return nades'

getNade :: Int64 -> App Nade
getNade key = do
  nade <- runDb . get . toSqlKey $ key
  case nade of Nothing -> throwWrapped err404
               Just n -> return . nadeFromDb' key $ n

postNade :: Nade' -> Maybe Text -> App Nade
postNade nade =
  withCookieText
  (\(CookieData{..}) -> do
      key <- runDb . insert . nadeToDb' _cookieDataUserId $ nade
      return . dbFill _cookieDataUserId (fromSqlKey key) $ nade
  )

-- TODO: check author using SQL query
putNade :: Int64 -> Nade' -> Maybe Text -> App Nade
putNade key nade =
  withCookieText
  (\(CookieData{..}) -> do
      oldNade <- getNade key
      when (_dbFilledAuthor oldNade /= _cookieDataUserId) $
        throwWrapped err403 -- only can change your own nades
      let nade' = dbFill _cookieDataUserId key nade
      void . runDb . replace (toSqlKey key) . nadeToDb $ nade'
      return nade'
  )

deleteNade :: Int64 -> Maybe Text -> App ()
deleteNade key =
  withCookieText
  (\(CookieData{..}) -> do
      void . runDb $ delete $ from $ \n -> do
        where_ (n ^. DN.NadeId ==. val (toSqlKey key)
                &&. n ^. DN.NadeAuthorId ==. val _cookieDataUserId)
  )

collapseNadeListings :: (Eq a) => [(a, b)] -> [(a, [b])]
collapseNadeListings = fmap f . L.groupBy ((==) `F.on` fst)
  where f nades = (fst . head $ nades, fmap snd nades)

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

postNadeList :: NadeList' -> Maybe Text -> App NadeList
postNadeList nadeList =
  withCookieText
  (\(CookieData{..}) -> do
      nadeListKey <- runDb $ insert . nadeListToDb' _cookieDataUserId $ nadeList
      let key = fromSqlKey nadeListKey
      return $ dbFill _cookieDataUserId key . convertNadeList' [] $ nadeList
  )

type NadeListingFilter = SqlExpr (Entity DN.NadeListing) -> SqlQuery ()

nlNLGFilter' :: Key DN.NadeList -> NadeListingFilter
nlNLGFilter' nadeListKey nlg =
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

putNadeList :: Int64 -> NadeList' -> Maybe Text -> App NadeList'
putNadeList key nadeList =
  withCookieText
  (\(CookieData{..}) -> do
      runDb . update $ (\nl -> do
                           setNadeList' nadeList nl
                           authorKeyNLFilter _cookieDataUserId (toSqlKey key) nl
                       )
      return nadeList
  )

deleteNadeList :: Int64 -> Maybe Text -> App ()
deleteNadeList key =
  withCookieText
  (\(CookieData{..}) -> do
      runDb $ do
        let key' = toSqlKey key
        deleted <- deleteCount $ from $ authorKeyNLFilter _cookieDataUserId key'
        when (deleted > 0) $ delete $ from $ nlNLGFilter' key'
  )

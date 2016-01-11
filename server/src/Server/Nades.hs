{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Nades where

import API.Nades
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Int
import qualified Data.Function as F
import qualified Data.List as L
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
  Nade nadeAuthorId nadeImages nadeTitle nadeDescription nadeTags key

nadeListFromDb :: Entity DN.NadeList -> [Entity DN.Nade] -> NadeList
nadeListFromDb nadeList nades =
  nadeListFromDb' key (entityVal nadeList) (fmap nadeFromDb nades)
  where key = fromSqlKey . entityKey $ nadeList

nadeListFromDb' :: Int64 -> DN.NadeList -> [Nade] -> NadeList
nadeListFromDb' key DN.NadeList{..} nades =
  NadeList nadeListAuthorId nadeListTitle nadeListDescription nades key

nadeToDb :: Nade -> (Key DN.Nade, DN.Nade)
nadeToDb nade = (toSqlKey . _nadeKey $ nade, nadeToDb' nade)

nadeToDb' :: Nade -> DN.Nade
nadeToDb' Nade{..} =
  DN.Nade _nadeAuthor _nadeImages _nadeTitle _nadeDescription _nadeTags

nadeServer :: ServerT NadeAPI App
nadeServer =
  allNades :<|> postNade :<|> getNade :<|> putNade :<|> deleteNade
  :<|> withCookieText myNades
  :<|> getNadeLists
  :<|> withCookieText myNadeLists

allNades :: App [Nade]
allNades = do
  nades <- runDb $ select $ from $ return
  let nades' = fmap nadeFromDb nades
  return nades'

getNade :: Int64 -> App Nade
getNade key = do
  nade <- runDb . get . toSqlKey $ key
  case nade of Nothing -> throwWrapped err404
               Just n -> return . nadeFromDb' key $ n

postNade :: Nade -> Maybe Text -> App Nade
postNade nade =
  withCookieText
  (\(CookieData{..}) -> do
      let nade' = nade{ _nadeAuthor = _cookieDataUserId }
      key <- runDb . insert . nadeToDb' $ nade'
      return nade'{ _nadeKey = fromSqlKey key }
      )

-- TODO: check author using SQL query
putNade :: Int64 -> Nade -> Maybe Text -> App Nade
putNade key nade =
  withCookieText
  (\(CookieData{..}) -> do
      oldNade <- getNade key
      when (_nadeAuthor oldNade /= _cookieDataUserId) $
        throwWrapped err403 -- only can change your own nades
      let nade' = nade{ _nadeAuthor = _cookieDataUserId
                      , _nadeKey = key }
      void . runDb . uncurry replace . nadeToDb $ nade'
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

myNades :: CookieData -> App [Nade]
myNades CookieData{..} = do
  nades <- runDb $ select $ from $ \n -> do
    where_ (n ^. DN.NadeAuthorId ==. val _cookieDataUserId)
    return n
  let nades' = fmap nadeFromDb nades
  return nades'

collapseNadeListings :: (Eq a) => [(a, b)] -> [(a, [b])]
collapseNadeListings = fmap f . L.groupBy ((==) `F.on` fst)
  where f nades = (fst . head $ nades, fmap snd nades)

getNadeLists :: App [NadeList]
getNadeLists = do
  nadeListings <- runDb $ select $ from $ \(n `InnerJoin` nlg `InnerJoin` nl) -> do
    on (nl ^. DN.NadeListId ==. nlg ^. DN.NadeListingNadeList)
    on (n ^. DN.NadeId ==. nlg ^. DN.NadeListingNade)
    orderBy [asc (nl ^. DN.NadeListId), asc (nlg ^. DN.NadeListingOrdinal)]
    return (nl, n)
  return . fmap (uncurry nadeListFromDb) . collapseNadeListings $ nadeListings

myNadeLists :: CookieData -> App [NadeList]
myNadeLists CookieData{..} = do
  nadeListings <- runDb $ select $ from $ \(n `InnerJoin` nlg `InnerJoin` nl) -> do
    on (nl ^. DN.NadeListId ==. nlg ^. DN.NadeListingNadeList)
    on (n ^. DN.NadeId ==. nlg ^. DN.NadeListingNade)
    where_ (nl ^. DN.NadeListAuthorId ==. val _cookieDataUserId)
    orderBy [asc (nl ^. DN.NadeListId), asc (nlg ^. DN.NadeListingOrdinal)]
    return (nl, n)
  return . fmap (uncurry nadeListFromDb) . collapseNadeListings $ nadeListings

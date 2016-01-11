{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Nades where

import API.Nades
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
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
nadeFromDb nade = nadeFromDb' key (entityVal nade)
  where key = decodeUtf8 . toStrict . encode $ entityKey nade

nadeFromDb' :: Text -> DN.Nade -> Nade
nadeFromDb' key DN.Nade{..} =
  Nade nadeAuthorId nadeImages nadeTitle nadeDescription nadeTags key

nadeListFromDb :: Entity DN.NadeList -> [Entity DN.Nade] -> NadeList
nadeListFromDb nadeList nades =
  nadeListFromDb' key (entityVal nadeList) (fmap nadeFromDb nades)
  where key = decodeUtf8 . toStrict . encode $ entityKey nadeList

nadeListFromDb' :: Text -> DN.NadeList -> [Nade] -> NadeList
nadeListFromDb' key DN.NadeList{..} nades =
  NadeList nadeListAuthorId nadeListTitle nadeListDescription nades key

nadeServer :: ServerT NadeAPI App
nadeServer = allNades :<|> withCookieText myNades :<|> nadeLists

allNades :: App [Nade]
allNades = do
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

collapseNadeListings :: (Eq a) => [(a, b)] -> [(a, [b])]
collapseNadeListings = fmap f . L.groupBy ((==) `F.on` fst)
  where f nades = (fst . head $ nades, fmap snd nades)

nadeLists :: App [NadeList]
nadeLists = do
  nadeListings <- runDb $ select $ from $ \(n `InnerJoin` nlg `InnerJoin` nl) -> do
    on (nl ^. DN.NadeListId ==. nlg ^. DN.NadeListingNadeList)
    on (n ^. DN.NadeId ==. nlg ^. DN.NadeListingNade)
    orderBy [asc (nl ^. DN.NadeListId), asc (nlg ^. DN.NadeListingOrdinal)]
    return (nl, n)
  return . fmap (uncurry nadeListFromDb) . collapseNadeListings $ nadeListings

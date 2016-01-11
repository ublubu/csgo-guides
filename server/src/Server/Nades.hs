{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Nades where

import API.Nades
import Data.Text (Text)
import qualified Database.Nades as DN
import Database.Persist (selectList, Entity(..))
import Servant
import Servant.Server

import API.SignIn
import Server.App
import Server.SignIn

nadeFromDb :: DN.Nade -> (Text, Nade)
nadeFromDb DN.Nade{..} =
  (nadeAuthorId, Nade nadeImages nadeTitle nadeDescription nadeTags)

nadeServer :: ServerT NadeAPI App
nadeServer = allNades :<|> withCookieText myNades

allNades :: App [Nade]
allNades = do
  nades <- runDb $ selectList [] []
  let nades' = map (snd . nadeFromDb . entityVal) nades
  return nades'

myNades :: CookieData -> App [Nade]
myNades = const allNades

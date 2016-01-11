{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Nades where

import API.Nades
import Data.Text (Text)
import qualified Database.Nades as DN
import Database.Persist (selectList, Entity(..))
import Servant
import Servant.Server

import Server.App

nadeFromDb :: DN.Nade -> (Text, Nade)
nadeFromDb DN.Nade{..} =
  (nadeAuthorId, Nade nadeImages nadeDescription nadeTags)

nadeServer :: ServerT NadeAPI App
nadeServer = allNades

allNades :: App [Nade]
allNades = do
  nades <- DN.runDb $ selectList [] []
  let nades' = map (snd . nadeFromDb . entityVal) nades
  return nades'

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Nades where

import Control.Monad
import Data.Aeson
import Data.Int
import Data.Text (Text)
import GHC.Generics
import Servant.API

import API.SignIn

data Nade = Nade { _nadeAuthor :: Text
                 , _nadeImages :: [Text]
                 , _nadeTitle :: Text
                 , _nadeDescription :: Maybe Text
                 , _nadeTags :: [Text]
                 , _nadeKey :: Int64
                 } deriving (Show, Eq, Generic)

data NadeList = NadeList { _nadeListAuthor :: Text
                         , _nadeListTitle :: Text
                         , _nadeListDescription :: Maybe Text
                         , _nadeListNades :: [Nade]
                         , _nadeListKey :: Int64
                         } deriving (Show, Eq, Generic)

instance FromJSON Nade
instance ToJSON Nade

instance FromJSON NadeList
instance ToJSON NadeList

type NadeAPI =
  "nades" :> Get '[JSON] [Nade]
  :<|> "nades" :> ReqBody '[JSON] Nade :> Cookied :> Post '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> Get '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> ReqBody '[JSON] Nade :> Cookied :> Put '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> Cookied :> Delete '[JSON] ()
  :<|> "myNades" :> Cookied :> Get '[JSON] [Nade]
  :<|> "nadeLists" :> Get '[JSON] [NadeList]
  :<|> "myNadeLists" :> Cookied :> Get '[JSON] [NadeList]

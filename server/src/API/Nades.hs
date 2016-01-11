{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Nades where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

data Nade = Nade { _nadeAuthor :: Text
                 , _nadeImages :: [Text]
                 , _nadeTitle :: Text
                 , _nadeDescription :: Maybe Text
                 , _nadeTags :: [Text]
                 , _nadeKey :: Text
                 } deriving (Show, Eq, Generic)

data NadeList = NadeList { _nadeListAuthor :: Text
                         , _nadeListTitle :: Text
                         , _nadeListDescription :: Maybe Text
                         , _nadeListNades :: [Nade]
                         , _nadeListKey :: Text
                         } deriving (Show, Eq, Generic)

instance FromJSON Nade
instance ToJSON Nade

instance FromJSON NadeList
instance ToJSON NadeList

type NadeAPI = "nades" :> Get '[JSON] [Nade]
               :<|> "my-nades" :> Header "Cookie" Text :> Get '[JSON] [Nade]
               :<|> "nadeLists" :> Get '[JSON] [NadeList]

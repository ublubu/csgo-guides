{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module API.Nades where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

data Nade = Nade { _nadeImages :: [Text]
                 , _nadeDescription :: Text
                 , _nadeTags :: [Text]
                 } deriving (Show, Eq, Generic)

instance FromJSON Nade
instance ToJSON Nade

type NadeAPI = "nades" :> Get '[JSON] [Nade]

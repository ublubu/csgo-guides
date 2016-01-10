{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module API.SignIn where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Servant

data CookieData = CookieData { _cookieDataUserId :: Text } deriving (Show, Eq)

instance FromJSON CookieData where
  parseJSON (Object v) = CookieData <$> v .: "userid"
  parseJSON _ = mzero

instance ToJSON CookieData where
  toJSON (CookieData userid) = object ["userid" .= userid]

type SetCookied a = Headers '[Header "Set-Cookie" Text] a
type SignInAPI = "tokensignin" :> QueryParam "idtoken" Text :> Get '[JSON] (SetCookied Text)
                 :<|> "cookiedata" :> Header "Cookie" Text :> Get '[JSON] CookieData

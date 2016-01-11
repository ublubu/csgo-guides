{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.SignIn where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

data CookieData = CookieData { _cookieDataUserId :: Text } deriving (Show, Eq, Generic)

instance FromJSON CookieData
instance ToJSON CookieData

type SetCookied a = Headers '[Header "Set-Cookie" Text] a
type SignInAPI = "tokensignin" :> QueryParam "idtoken" Text :> Get '[JSON] (SetCookied Text)
                 :<|> "cookiedata" :> Header "Cookie" Text :> Get '[JSON] CookieData

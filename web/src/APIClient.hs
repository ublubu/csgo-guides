{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module APIClient where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

import API.SignIn

signInApi :: Proxy SignInAPI
signInApi = Proxy

tokensignin :: Maybe Text -> EitherT ServantError IO (SetCookied Text)
idtoken :: Maybe Text -> EitherT ServantError IO CookieData

tokensignin :<|> idtoken = client signInApi (Just $ BaseUrl Http "localhost" 8081)

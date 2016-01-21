{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module APIClient where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

import API.SignIn
import API.Nades

type ServIO = EitherT ServantError IO

baseUrl :: Maybe BaseUrl
baseUrl = (Just $ BaseUrl Http "localhost" 8081)

signInApi :: Proxy ("api" :> SignInAPI)
signInApi = Proxy

tokensignin :: Maybe Text -> ServIO (SetCookied Text)
idtoken :: Maybe Text -> ServIO CookieData

tokensignin :<|> idtoken = client signInApi baseUrl

nadeApi :: Proxy ("api" :> NadeAPI)
nadeApi = Proxy

getNades :: ServIO [Nade]
postNade :: Nade' -> Maybe Text -> ServIO Nade
getNade :: Int64 -> ServIO Nade
putNade :: Int64 -> Nade' -> Maybe Text -> ServIO Nade
deleteNade :: Int64 -> Maybe Text -> ServIO ()
myNades :: Maybe Text -> ServIO [Nade]

getNades :<|> postNade :<|> getNade :<|> putNade :<|> deleteNade :<|> myNades =
  client nadeApi baseUrl

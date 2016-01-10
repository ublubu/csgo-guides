{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server

import API.SignIn
import Server.App
import Server.SignIn

type StaticAPI = Raw
type API = SignInAPI :<|> StaticAPI

api :: Proxy API
api = Proxy

server :: AppConfig -> Server API
server config = enter (Nat $ runApp config) signInServer :<|> serveDirectory (completeFilePath config "/static")

app :: AppConfig -> Application
app = serve api . server

runAppConfig :: AppConfig -> IO ()
runAppConfig = run 8081 . app

main :: IO ()
main = runAppConfig =<< defaultAppConfig "."

main' :: IO ()
main' = runAppConfig =<< defaultAppConfig ".."

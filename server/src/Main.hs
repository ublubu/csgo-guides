{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Servant
import Servant.Server

import API.Nades
import API.SignIn
import Server.App
import Server.Nades
import Server.SignIn

type StaticAPI = Raw
type API = (SignInAPI :<|> NadesAPI) :<|> StaticAPI

api :: Proxy API
api = Proxy

server :: AppConfig -> Server API
server config = f (signInServer :<|> nadesServer) :<|> serveDirectory (completeFilePath config "/static")
  where f = enter (Nat $ runApp config)

app :: AppConfig -> Application
app = serve api . server

runAppConfig :: AppConfig -> IO ()
runAppConfig config = do
  loadDb config
  run 8081 . logStdoutDev . app $ config

main :: IO ()
main = runAppConfig =<< defaultAppConfig "."

main' :: IO ()
main' = runAppConfig =<< defaultAppConfig ".."

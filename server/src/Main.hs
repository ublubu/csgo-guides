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

server' :: Server API
server' = enter (Nat runApp) signInServer :<|> serveDirectory "../static"

app :: Application
app = serve api $ server'

main :: IO ()
main = run 8081 app

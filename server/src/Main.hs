{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Network.HTTP.Client.Conduit (Manager, withManager, HasHttpManager(..), newManager)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server

data AppConfig = AppConfig { _appConfigHttpManager :: Manager }
data AppError = Invalid Text | WrappedServantErr ServantErr
type App = ReaderT AppConfig (ExceptT AppError IO)

instance HasHttpManager AppConfig where
  getHttpManager = _appConfigHttpManager

type ReaderAPI = "tokensignin" :> QueryParam "idtoken" String :> Get '[JSON] String
type StaticAPI = Raw
type API = ReaderAPI :<|> StaticAPI

api :: Proxy API
api = Proxy

readerServer :: ServerT ReaderAPI App
readerServer =
  tokensignin

  where tokensignin :: Maybe String -> App String
        tokensignin =
          maybe (throwError $ Invalid "missing token in queryparams") return

runAppT :: App a -> EitherT ServantErr IO a
runAppT action = do
  config <- defaultAppConfig
  res <- liftIO . runExceptT . flip runReaderT config $ action
  EitherT $ return $ case res of
    Left (Invalid text) -> Left err400 { errBody = textToBSL text }
    Left (WrappedServantErr e) -> Left e
    Right a -> Right a

textToBSL :: Text -> ByteString
textToBSL = encodeUtf8 . fromStrict

defaultAppConfig :: (MonadIO m) => m AppConfig
defaultAppConfig = fmap AppConfig newManager

server' :: Server API
server' = enter (Nat runAppT) readerServer :<|> serveDirectory "static"

app :: Application
app = serve api $ server'

main :: IO ()
main = run 8081 app

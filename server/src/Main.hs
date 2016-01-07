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
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server

data AppConfig = AppConfig
data AppError = Invalid Text | WrappedServantErr ServantErr
newtype App a = App { runApp :: ReaderT AppConfig (ExceptT AppError IO) a
                    } deriving ( Monad
                               , Functor
                               , Applicative
                               , MonadReader AppConfig
                               , MonadError AppError
                               , MonadIO)

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

fromAppT :: EitherT ServantErr IO a -> App a
fromAppT action = do
  res <- liftIO $ runEitherT action
  App . ReaderT . const . ExceptT . return $ mapLeft WrappedServantErr res

runAppT :: AppConfig -> App a -> EitherT ServantErr IO a
runAppT config action = do
  res <- liftIO $ runExceptT $ runReaderT (runApp action) config
  EitherT $ return $ case res of
    Left (Invalid text) -> Left err400 { errBody = textToBSL text }
    Left (WrappedServantErr e) -> Left e
    Right a -> Right a

textToBSL :: Text -> ByteString
textToBSL = encodeUtf8 . fromStrict

server' :: AppConfig -> Server API
server' config = enter (Nat $ runAppT config) readerServer :<|> serveDirectory "static"

app :: Application
app = serve api $ server' AppConfig

main :: IO ()
main = run 8081 app

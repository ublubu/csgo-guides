{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Server.App where

import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager(..), newManager)
import Servant
import Servant.Server
import Web.ClientSession

data AppConfig = AppConfig { _appConfigHttpManager :: Manager
                           , _appConfigGoogleClientId :: Text
                           , _appConfigClientSessionKey :: Key
                           , _appConfigFileRoot :: FilePath }
data AppError = Invalid Text | WrappedServantErr ServantErr
type App = ReaderT AppConfig (ExceptT AppError IO)

instance HasHttpManager AppConfig where
  getHttpManager = _appConfigHttpManager

runApp :: AppConfig -> App a -> EitherT ServantErr IO a
runApp config action = do
  res <- liftIO . runExceptT . flip runReaderT config $ action
  EitherT $ return $ case res of
    Left (Invalid text) -> Left err400 { errBody = encodeUtf8 . fromStrict $ text }
    Left (WrappedServantErr e) -> Left e
    Right a -> Right a

-- TODO: ssl for the Manager
defaultAppConfig :: (MonadIO m) => FilePath -> m AppConfig
defaultAppConfig fileRoot = do
  clientId <- liftIO . fmap T.strip . T.readFile $ fileRoot ++ "/clientid.txt"
  manager <- newManager
  key <- liftIO . getKey $ fileRoot ++ "/sessionkey.txt"
  return $ AppConfig manager clientId key fileRoot

completeFilePath :: AppConfig -> FilePath -> FilePath
completeFilePath config = (++) (_appConfigFileRoot config)

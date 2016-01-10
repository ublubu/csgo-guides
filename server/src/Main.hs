{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Either.Combinators (mapLeft)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Network.HTTP.Client.Conduit (Manager, withManager, HasHttpManager(..), newManager, withResponse, Response)
import qualified Network.HTTP.Client.Conduit as H
import qualified Network.HTTP.Types.Status as H
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server
import Web.ClientSession

data AppConfig = AppConfig { _appConfigHttpManager :: Manager
                           , _appConfigGoogleClientId :: Text
                           , _appConfigClientSessionKey :: Key }
data AppError = Invalid Text | WrappedServantErr ServantErr
type App = ReaderT AppConfig (ExceptT AppError IO)

instance HasHttpManager AppConfig where
  getHttpManager = _appConfigHttpManager

type SetCookied a = Headers '[Header "Set-Cookie" String] a
type ReaderAPI = "tokensignin" :> QueryParam "idtoken" String :> Get '[JSON] (SetCookied String)
                 :<|> "cookiedata" :> Header "Cookie" Text :> Get '[JSON] CookieData
type StaticAPI = Raw
type API = ReaderAPI :<|> StaticAPI

api :: Proxy API
api = Proxy

readerServer :: ServerT ReaderAPI App
readerServer =
  tokensignin :<|> cookiedata

  where tokensignin :: Maybe String -> App (SetCookied String)
        tokensignin token = case token of
          Nothing -> throwError $ Invalid "missing token in queryparams"
          Just t -> askGoogle t

        cookiedata :: Maybe Text -> App CookieData
        cookiedata = maybe (throwError $ Invalid "no cookie provided") (decryptCookie . unwrapCookie)

data GoogleTokenInfo = GoogleTokenInfo { _googleTokenAud :: Text
                                       , _googleTokenSub :: Text
                                       } deriving (Show, Eq)

instance FromJSON GoogleTokenInfo where
  parseJSON (Object v) = GoogleTokenInfo <$> v .: "aud" <*> v .: "sub"
  parseJSON _ = mzero

data CookieData = CookieData { _cookieDataUserId :: Text } deriving (Show, Eq)

instance FromJSON CookieData where
  parseJSON (Object v) = CookieData <$> v .: "userid"
  parseJSON _ = mzero

instance ToJSON CookieData where
  toJSON (CookieData userid) = object ["userid" .= userid]

cookiePrefix :: String
cookiePrefix = "Session="

wrapCookie :: String -> String
wrapCookie cookie = cookiePrefix ++ cookie

unwrapCookie :: Text -> Text
unwrapCookie = T.drop (length cookiePrefix)

getCookieData :: GoogleTokenInfo -> App CookieData
getCookieData = return . CookieData . _googleTokenSub

encryptCookie :: CookieData -> App String
encryptCookie cookie = do
  iv <- liftIO randomIV
  config <- ask
  return . BS.unpack $ encrypt (_appConfigClientSessionKey config) iv (BSL.toStrict $ encode cookie)

decryptCookie :: Text -> App CookieData
decryptCookie cookieText = do
  config <- ask
  let key = _appConfigClientSessionKey config
      cookieBytes = T.encodeUtf8 cookieText
  cookie <- maybe (throwError $ Invalid "couldn't decrypt cookie") return (decrypt key cookieBytes)
  maybe (throwError $ Invalid "couldn't parse cookie") return . decodeStrict' $ cookie

askGoogle :: String -> App (SetCookied String)
askGoogle token = do
  request <- liftIO $ H.parseUrl "https://www.googleapis.com/oauth2/v3/tokeninfo"
  let r = H.setQueryString [("id_token", Just $ BS.pack token)] request
  tokenInfo <- withResponse r processGoogleResponse
  cookie <- encryptCookie =<< getCookieData tokenInfo
  return $ addHeader (wrapCookie cookie) (unpack . _googleTokenSub $ tokenInfo)

processGoogleResponse :: H.Response (ConduitM () BS.ByteString App ()) -> App GoogleTokenInfo
processGoogleResponse res = do
  let bodyProducer = H.responseBody res
  body <- runConduit $ bodyProducer =$= C.fold mappend mempty
  when (H.responseStatus res /= H.status200) $ throwError
    $ Invalid "google says your token is no good"
  let tokenInfo = (decodeStrict' body :: Maybe GoogleTokenInfo)
  case tokenInfo of
    Nothing -> throwError $ Invalid "google's response wasn't token info"
    Just t -> do
      config <- ask
      when (_appConfigGoogleClientId config /= _googleTokenAud t)
        $ throwError $ Invalid "your token isn't for this app"
      return t

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

-- TODO: ssl for the Manager
defaultAppConfig :: (MonadIO m) => m AppConfig
defaultAppConfig = do
  clientId <- liftIO . fmap T.strip $ T.readFile "../clientid.txt"
  manager <- newManager
  key <- liftIO $ getKey "../sessionkey.txt"
  return $ AppConfig manager clientId key

server' :: Server API
server' = enter (Nat runAppT) readerServer :<|> serveDirectory "../static"

app :: Application
app = serve api $ server'

main :: IO ()
main = run 8081 app

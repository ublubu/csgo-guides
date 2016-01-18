{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module GoogleSignIn where

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef)
import Reflex.Dom

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Dependent.Map (DSum(..))
import qualified Data.JSString as JSS
import Data.Monoid
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types (JSVal)

import Utils

clientId :: String
clientId = "808800165858-1ma83vrqlk94apianbbe2magdp8vado0.apps.googleusercontent.com"

headEl :: (MonadWidget t m) => m ()
headEl = do
  elAttr "meta" ("name" =: "google-signin-client_id"
                <> "content" =: clientId) noContents
  elAttr "script" ("src" =: "https://apis.google.com/js/platform.js"
                  <> "async" =: "async"
                  <> "defer" =: "defer") noContents

callbackEvent :: (MonadWidget t m) => m (Event t JSVal)
callbackEvent = do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    (eSignIn, eSignInTriggerRef) <- newEventWithTriggerRef
    let onSignIn :: JSVal -> IO ()
        onSignIn val = postGui $ do
          mt <- readRef eSignInTriggerRef
          forM_ mt $ \t -> runWithActions [t :=> val]
        setOnSignIn =
          setGoogleSignInCallback =<< (syncCallback1 ContinueAsync onSignIn)
    schedulePostBuild . liftIO $ setOnSignIn
    return eSignIn

foreign import javascript unsafe "onGoogleSignIn_ = $1"
  setGoogleSignInCallback :: Callback a -> IO ()

signInButton :: (MonadWidget t m) => m ()
signInButton =
  elAttr "div" ("class" =: "g-signin2"
                <> "data-onsuccess" =: "onGoogleSignIn_"
                <> "data-theme" =: "dark") noContents

printGoogleSignIn :: JSVal -> IO ()
printGoogleSignIn jsval = do
  Just obj <- fromJSVal jsval :: IO (Maybe AE.Value)
  putStrLn . BSL.unpack . AE.encode $ obj

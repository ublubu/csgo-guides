{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reflex
import Reflex.Dom
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import APIClient
import NadeListWidget (appWidget)
import Style
import qualified Styles as S
import Utils

import Control.Monad.Ref
import qualified Data.Aeson as AE
import GHCJS.Marshal (fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import qualified Data.JSString as JSS
import GHCJS.Types (JSVal)
import Reflex.Host.Class
import Data.Dependent.Map (DSum(..))
import Control.Monad.IO.Class
import Control.Monad

printGoogleSignIn :: JSVal -> IO ()
printGoogleSignIn jsval = do
  Just obj <- fromJSVal jsval :: IO (Maybe AE.Value)
  putStrLn . BSL.unpack . AE.encode $ obj

foreign import javascript unsafe "onGoogleSignIn_ = $1"
  setGoogleSignInCallback :: Callback a -> IO ()

main :: IO ()
main = do
  mainWidgetWithHead headEl $ do
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
    performEvent_ (fmap (liftIO . printGoogleSignIn) eSignIn)
    elAttr "div" ("class" =: "g-signin2"
                  <> "data-onsuccess" =: "onGoogleSignIn_"
                  <> "data-theme" =: "dark") noContents

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

googleClientId :: String
googleClientId = "808800165858-1ma83vrqlk94apianbbe2magdp8vado0.apps.googleusercontent.com"

headEl :: (MonadWidget t m) => m ()
headEl = do
  elAttr "meta" ("name" =: "google-signin-client_id"
                <> "content" =: googleClientId) noContents
  elAttr "script" ("src" =: "https://apis.google.com/js/platform.js"
                  <> "async" =: "async"
                  <> "defer" =: "defer") noContents
  el "style" $ text css

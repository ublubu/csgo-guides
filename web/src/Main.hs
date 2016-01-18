{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Data.Monoid

import APIClient
import NadeListWidget (appWidget)
import Style
import qualified Styles as S
import Utils

import qualified GoogleSignIn as GSI

main :: IO ()
main = do
  mainWidgetWithHead headEl $ do
    signIns <- GSI.callbackEvent
    performEvent_ (fmap (liftIO . GSI.printGoogleSignIn) signIns)
    GSI.signInButton

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  GSI.headEl
  el "style" $ text css

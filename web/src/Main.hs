{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Sequence as SQ

import NadeListWidget (appWidget)
import Style
import qualified Styles as S
import Utils
import Forms
import NadeForm

import qualified GoogleSignIn as GSI
import qualified SignIn as SI

main :: IO ()
main = do
  mainWidgetWithHead headEl $ do
    signIns <- SI.signInEvent
    performEvent_ (fmap (liftIO . print) signIns)
    GSI.signInButton
    images <- imagesForm (SQ.fromList ["asdf", "1234"])
    performEvent_ (fmap (liftIO . print) (updated images))

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  GSI.headEl
  el "style" $ text css

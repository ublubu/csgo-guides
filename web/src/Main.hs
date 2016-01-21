{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Monoid

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
    nades <- nadeForm emptyNade
    performEvent_ (fmap (liftIO . print) (updated nades))

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  GSI.headEl
  el "style" $ text css

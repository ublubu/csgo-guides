{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Monoid

import NadeListWidget (nadesViewer)
import Style
import qualified Styles as S
import Utils
import Forms
import NadeForm

import qualified GoogleSignIn as GSI
import qualified SignIn as SI

import Combinators
import CommonWidgets
import Utils

main :: IO ()
main = do
  mainWidgetWithHead headEl $ do
    signIns <- SI.signInEvent
    performEvent_ (fmap (liftIO . print) signIns)
    GSI.signInButton
    editMode <- toggleButton False "Edit" "View"
    (nades, _) <- ewhen signIns $ do
      dif editMode (updated <$> myNadesForm) (return ())
    performEvent_ (fmap (liftIO . print) nades)

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  GSI.headEl
  el "style" $ text css

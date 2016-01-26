{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
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

import API.Nades
import Combinators
import CommonWidgets
import Utils

main :: IO ()
main = do
  mainWidgetWithHead headEl $ mdo
    signIns <- SI.signInEvent
    performEvent_ (fmap (liftIO . print) signIns)
    GSI.signInButton
    editMode <- toggleButton True "Edit" "View"
    (nades, _) <- ewhen signIns $ do
      nades' <- holdDyn [] $ fmap (fmap _dbFilledContents) nades
      let editor = updated <$> myNadesForm
          viewer = ecDyn' nadesViewer nades'
      dif editMode editor viewer
    return ()

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  GSI.headEl
  el "style" $ text css

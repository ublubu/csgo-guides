module Main where

import Reflex
import Reflex.Dom
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import APIClient
import NadeListWidget (appWidget)
import Style
import qualified Styles as S

main :: IO ()
main =
  mainWidgetWithCss css appWidget

css :: BS.ByteString
css = BS.pack . toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

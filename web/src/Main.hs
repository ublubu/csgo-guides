{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Text

main :: IO ()
main = mainWidget appWidget

appWidget :: (MonadWidget t m) => m ()
appWidget = el "h1" $ do
  buttonEvent <- button "click here"
  let req = xhrRequest "GET" "public/blah.txt" def
  res <- performRequestAsync (tag (constant req) buttonEvent)
  contents <- holdDyn "hello" $ fmap (show . _xhrResponse_body) res
  dynText contents

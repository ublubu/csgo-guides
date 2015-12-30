{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Text
import Text.Parsec
import Data.Either.Combinators (rightToMaybe)
import Control.Monad
import CsgoGuideParser

main :: IO ()
main = mainWidget appWidget

appWidget :: (MonadWidget t m) => m ()
appWidget = el "h1" $ do
  bld <- getPostBuild
  nades_ <- getNadeInfos (tag (constant "public/cache.txt") bld)
  nades <- holdDyn [] nades_
  nadeWidgets <- mapDyn (mapM_ nadeInfoWidget) nades
  void . dyn $ nadeWidgets

getResource :: (MonadWidget t m) => Event t String -> m (Event t Text)
getResource address = do
  res <- performRequestAsync $ fmap makeReq address
  return $ fmapMaybe _xhrResponse_body res
  where makeReq addr = xhrRequest "GET" addr def

getNadeInfos :: (MonadWidget t m) => Event t String -> m (Event t [NadeInfo])
getNadeInfos address = do
  contents <- getResource address
  let nades = fmap (runParser nadeInfos () "" . unpack) contents
  return $ fmapMaybe rightToMaybe nades

nadeInfoWidget :: (MonadWidget t m) => NadeInfo -> m ()
nadeInfoWidget (NadeInfo img desc tags) = do
  el "div" $ do
    elAttr "img" ("src" =: img) (return ())
    el "div" $ text desc

{-# LANGUAGE OverloadedStrings #-}

module NadeForm where

import Reflex
import Reflex.Dom

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import Forms

imagesForm :: (MonadWidget t m) => Map Int Text -> m (Dynamic t (Map Int Text))
imagesForm initVals = listForm (simpleListAppendButton "") simpleTextListItem initVals

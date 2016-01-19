{-# LANGUAGE OverloadedStrings #-}

module NadeForm where

import Reflex
import Reflex.Dom

import Data.Sequence (Seq)
import Data.Text (Text)

import Forms

imagesForm :: (MonadWidget t m) => Seq Text -> m (Dynamic t (Seq Text))
imagesForm initVals = listForm "" textListItem initVals

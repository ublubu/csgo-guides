{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NadeForm where

import Reflex
import Reflex.Dom

import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import API.Nades

import APIClient
import APIForms
import Forms

emptyNade :: Nade'
emptyNade =
  Nade' [] "" Nothing []

nadeForm :: (MonadWidget t m) => Nade' -> m (Dynamic t Nade')
nadeForm Nade'{..} = do
  images <- simpleTextListForm _nadeImages
  title <- textForm _nadeTitle
  description <- maybeTextForm _nadeDescription
  tags <- simpleTextListForm _nadeTags
  mapDyn Nade' images >>= f title >>= f description >>= f tags
  where f = combineDyn (flip ($))

postNadeForm :: (MonadWidget t m) => m (Event t Nade)
postNadeForm =
  simpleApiForm (flip postNade Nothing) (nadeForm emptyNade)

convertNade :: Nade -> (Int64, Nade')
convertNade n = (_dbFilledKey n, _dbFilledContents n)

createEditNadeForm :: (MonadWidget t m) => m (Event t (Either () Nade))
createEditNadeForm =
  createEditForm (flip postNade Nothing) (nadeForm emptyNade)
  convertNade (\k v -> putNade k v Nothing) nadeForm
  (\k -> deleteNade k Nothing)

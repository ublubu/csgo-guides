{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Reflex
import Reflex.Dom

import Control.Monad
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as SS
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T

import Layout
import Style
import qualified Styles as S
import Utils

type ListUpdater k v = Map k v -> Map k v
type ListControl k t v m = m (Event t (ListUpdater k v))
type ListItemControl k t v m = k -> Dynamic t v -> ListControl k t v m

class (Ord k) => ListKey k where
  nextKey :: k -> k
  minKey :: k

instance ListKey Int where
  nextKey = (+1)
  minKey = 0

convertList :: (ListKey k) => [v] -> Map k v
convertList = M.fromList . zip keys
  where keys = iterate nextKey minKey

nextListKey :: (ListKey k) => Map k v -> k
nextListKey items
  | M.null items = minKey
  | otherwise = nextKey . fst . M.findMax $ items

updateOne :: (Ord k) => k -> v -> ListUpdater k v
updateOne = M.insert

stringListItem :: (MonadWidget t m, Ord k) => (v -> String) -> (String -> v) -> ListItemControl k t v m
stringListItem toString fromString key dVal = do
  dVal' <- mapDyn toString dVal
  init <- sample $ current dVal'
  ti <- textInput def{
    _textInputConfig_initialValue = init,
    _textInputConfig_setValue = updated dVal'
    }
  return $ fmap (M.insert key . fromString) (_textInput_input ti)

listItemDelete :: (MonadWidget t m, Ord k) => ListItemControl k t v m
listItemDelete key _ = do
  deleteEvents <- button "-"
  return $ fmap (const $ M.delete key) deleteEvents

simpleStringListItem :: (MonadWidget t m, Ord k) => (v -> String) -> (String -> v) -> ListItemControl k t v m
simpleStringListItem toString fromString key dVal = do
  as <- stringListItem toString fromString key dVal
  bs <- listItemDelete key dVal
  return $ leftmost [as, bs]

simpleTextListItem :: (MonadWidget t m, Ord k) => ListItemControl k t Text m
simpleTextListItem = simpleStringListItem unpack pack

applyUpdates :: (Ord k) => Map k (ListUpdater k v) -> Map k v -> Map k v
applyUpdates updaters init =
  F.foldl' (flip ($)) init updaters

applyUpdate :: (Ord k) => ListUpdater k v -> Map k v -> Map k v
applyUpdate = ($)

listForm :: (MonadWidget t m, ListKey k) => ListControl k t v m -> ListItemControl k t v m -> Map k v -> m (Dynamic t (Map k v))
listForm listControls itemRenderer initVals = mdo
  itemUpdaters <- listViewWithKey dVals itemRenderer
  listUpdaters <- listControls
  let updaters = leftmost [fmap Left itemUpdaters, fmap Right listUpdaters]
  dVals <- foldDyn (mapEither applyUpdates applyUpdate) initVals updaters
  return dVals

simpleListAppendButton :: (MonadWidget t m, ListKey k) => v -> ListControl k t v m
simpleListAppendButton emptyItem = do
  addEvents <- button "+"
  return $ fmap (const $ appendListItem emptyItem) addEvents

appendListItem :: (ListKey k) => v -> Map k v -> Map k v
appendListItem item items =
  M.insert (nextListKey items) item items

textForm :: (MonadWidget t m) => Text -> m (Dynamic t Text)
textForm initVal = do
  ti <- textInput def{
    _textInputConfig_initialValue = unpack initVal
    }
  mapDyn pack $ _textInput_value ti

maybeTextForm :: (MonadWidget t m) => Maybe Text -> m (Dynamic t (Maybe Text))
maybeTextForm initVal =
  mapDyn toMaybe =<< textForm initVal'
  where initVal' = fromMaybe "" initVal
        toMaybe x
          | T.null x = Nothing
          | otherwise = Just x

simpleTextListForm :: (MonadWidget t m) => [Text] -> m (Dynamic t [Text])
simpleTextListForm initVals =
  mapDyn M.elems =<< listForm (simpleListAppendButton "") simpleTextListItem initVals'
  where initVals' = convertList initVals :: Map Int Text

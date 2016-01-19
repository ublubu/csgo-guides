{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forms where

import Reflex
import Reflex.Dom

import Control.Monad
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence (Seq, (|>))
import Data.Text (Text, unpack, pack)
import qualified Data.Sequence as SQ
import qualified Data.Traversable as T

import Layout
import Style
import qualified Styles as S
import Utils

type WidgetMaker t m a = Seq a -> Int -> m (Event t (Seq a))

makeWidgets :: forall t m a . (MonadWidget t m) => WidgetMaker t m a -> Seq a -> m (Event t (Seq a))
makeWidgets makeWidget = fmap seqLeftmost . T.mapM (uncurry makeWidget) . indexedVals
  where indexedVals :: Seq a -> Seq (Seq a, Int)
        indexedVals vs = SQ.mapWithIndex (\i _ -> (vs, i)) vs

listForm :: (MonadWidget t m) => a -> WidgetMaker t m a -> Seq a -> m (Dynamic t (Seq a))
listForm emptyVal makeWidget initVals = mdo
  let addVal = modEvent' (|> emptyVal) vals addEvent
  widgets <- mapDyn (makeWidgets makeWidget) vals
  widgetEvents <- dynWidgetEvents widgets
  vals <- holdDyn initVals (leftmost [addVal, widgetEvents])
  addEvent <- button "+"
  return vals

stringListItem :: forall t m a . (MonadWidget t m) => (a -> String) -> (String -> a) -> WidgetMaker t m a
stringListItem unpack pack vals i = do
  let init =  unpack $ SQ.index vals i
  ti <- textInput def{_textInputConfig_initialValue = init}
  let textChanged = updated $ _textInput_value ti
      f :: Event t String -> Event t (Seq a)
      f = fmap (flip (SQ.update i) vals . pack)
  return . f $ textChanged

textListItem :: (MonadWidget t m) => WidgetMaker t m Text
textListItem = stringListItem unpack pack

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module NadeListWidget where

import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Text (pack, unpack, Text)
import Data.Either.Combinators (rightToMaybe)
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as Set

import API.Nades
import Layout
import Style
import qualified Styles as S
import Utils

nadesViewer :: (MonadWidget t m) => [Nade'] -> m ()
nadesViewer nades = do
  let allTags = (mconcat . fmap (Set.fromList . _nadeTags)) nades
  checkedTags <- nadeTagSelector allTags
  selectedNades <- mapDyn (flip filterByTag nades) checkedTags
  nadeWidgets <- mapDyn (mapM nadeInfoWidget) selectedNades
  nadeImgClicks <- eventJoin =<< (fmap (fmap leftmost) . dyn $ nadeWidgets)
  performEvent_ $ fmap (\img -> liftIO $ print img) nadeImgClicks
  nadeOverlayWidget nadeImgClicks

filterByTag :: Set.Set Text -> [Nade'] -> [Nade']
filterByTag tags = filter $ hasAllTags tags

nadeTagsIn :: Set.Set Text -> Nade' -> Bool
nadeTagsIn tags = any (`Set.member` tags) . _nadeTags

hasAllTags :: Set.Set Text -> Nade' -> Bool
hasAllTags tags nade = all (`elem` _nadeTags nade) tags

nadeThumb :: (MonadWidget t m) => Text -> m (Event t Text)
nadeThumb src = do
  (elem, _) <- elAttr' "img" ("src" =: unpack src <> toAttr (S.width' "25%")) noContents
  return . fmap (const src) $ domEvent Click elem

nadeFullscreen :: (MonadWidget t m) => Text -> m (Event t ())
nadeFullscreen src = do
  (elem, _) <- elAttr' "div" (toAttr $ "background-image" =: ("url(" ++ unpack src ++ ")")
                             <> "background-repeat" =: "no-repeat"
                             <> "background-size" =: "contain"
                             <> "background-position" =: "center"
                             <> S.width' "100vw"
                             <> S.height' "100vh"
                             ) noContents
  return $ domEvent Click elem

nadeInfoWidget :: (MonadWidget t m) => Nade' -> m (Event t Text)
nadeInfoWidget Nade'{..} = do
  elAttr "div" (toAttr $ "border-top" =: "2px solid black") $ do
    imgClicks <- mapM nadeThumb _nadeImages
    elAttr "div" (toAttr $ "font-size" =: "1em"
                 <> "padding" =: "0 0.5em 0.7em 0.5em"
                 ) $ text (unpack _nadeTitle)
    return . leftmost $ imgClicks

nadeTagCheckbox :: (MonadWidget t m) => Text -> Bool -> m (Checkbox t)
nadeTagCheckbox tag startChecked =
  elAttr "label" (toAttr $ S.displayInlineBlock) $ do
    cb <- checkbox startChecked def{_checkboxConfig_attributes =
                                    constDyn . toAttr $ "vertical-align" =: "middle"
                                   }
    text $ unpack tag
    return cb

toTagSetDyn :: (MonadWidget t m) => (Text, Dynamic t Bool) -> m (Dynamic t (Set.Set Text))
toTagSetDyn (tag, checked) = mapDyn (toTagSet tag) checked

toTagSet :: Text -> Bool -> Set.Set Text
toTagSet tag checked = if checked then Set.singleton tag else Set.empty

renderTagSelector :: (MonadWidget t m) => Dynamic t Bool -> Text -> m (Text, Dynamic t Bool)
renderTagSelector startChecked tag = fmap (\r -> (tag, r)) $ renderTagSelector' False startChecked tag

renderTagSelector' :: (MonadWidget t m) => Bool -> Dynamic t Bool -> Text -> m (Dynamic t Bool)
renderTagSelector' initStartChecked startChecked tag = dynWidgetDyn' False (fmap _checkbox_value . nadeTagCheckbox tag) startChecked

nadeTagSelector :: (MonadWidget t m) => Set.Set Text -> m (Dynamic t (Set.Set Text))
nadeTagSelector tags =
  elAttr "div" (toAttr $ "padding-top" =: "0.1em") $ do
    clearFilters <- button "clear filters"
    initTagCbVals <- holdDyn False $ fmap (const False) clearFilters
    tagCbs <- mapM (renderTagSelector initTagCbVals) (Set.toList tags) -- [(String, Dynamic t (Checkbox t))]
    tagCbVals <- mapM toTagSetDyn tagCbs -- [Dynamic t (Set String)]
    mconcatDyn tagCbVals

nadeOverlayWidget :: (MonadWidget t m) => Event t Text -> m ()
nadeOverlayWidget imgClicks = mdo
  imgDyn <- holdDyn Nothing (leftmost [fmap Just imgClicks, fmap (const Nothing) overlayClicks])
  overlayClicks <- dynWidgetEvents' maybeOverlay imgDyn
  return ()
  where maybeOverlay = maybe (return never) (overlay . nadeFullscreen)

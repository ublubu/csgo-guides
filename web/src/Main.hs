{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Text (pack, unpack, Text)
import Text.Parsec
import Data.Either.Combinators (rightToMaybe)
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import CsgoGuideParser (nadeInfos, NadeInfo(..))
import Style
import qualified Styles as S
import qualified Data.Set as Set

main :: IO ()
main =
  mainWidgetWithCss css appWidget

css :: BS.ByteString
css = BS.pack . toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

appWidget :: (MonadWidget t m) => m ()
appWidget = do
  bld <- getPostBuild
  nades_ <- getNadeInfos (tag (constant "public/cache.txt") bld)
  nades <- holdDyn [] nades_
  allTags <- mapDyn (mconcat . fmap (Set.fromList . _nadeTags)) nades
  checkedTags' <- dyn =<< mapDyn nadeTagSelector allTags
  checkedTags <- fmap joinDyn $ holdDyn (constDyn Set.empty) checkedTags'
  selectedNades <- combineDyn filterByTag checkedTags nades
  nadeWidgets <- mapDyn (mapM_ nadeInfoWidget) selectedNades
  void . dyn $ nadeWidgets

filterByTag :: Set.Set String -> [NadeInfo] -> [NadeInfo]
filterByTag tags = filter $ nadeTagsIn tags

nadeTagsIn :: Set.Set String -> NadeInfo -> Bool
nadeTagsIn tags = any (`Set.member` tags) . _nadeTags

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
  elAttr "div" (toAttr $ "border-top" =: "2px solid black") $ do
    elAttr "img" ("src" =: img <> toAttr S.fullWidth) (return ())
    elAttr "div" (toAttr $ "font-size" =: "2em"
                 <> "padding" =: "0.25em 0.5em 1.5em 0.5em"
                 ) $ text desc

nadeTagCheckbox :: (MonadWidget t m) => Bool -> String -> m (Checkbox t)
nadeTagCheckbox startChecked tag =
  elAttr "div" (toAttr $ S.displayInlineBlock) $ do
    cb <- checkbox startChecked def{_checkboxConfig_attributes =
                                    constDyn . toAttr $ "vertical-align" =: "middle"
                                   }
    text tag
    return cb

allTag :: String
allTag = "all"

toTagSetDyn :: (MonadWidget t m) => (String, Checkbox t) -> m (Dynamic t (Set.Set String))
toTagSetDyn (tag, cb) = mapDyn (toTagSet tag) $ _checkbox_value cb

toTagSet :: String -> Bool -> Set.Set String
toTagSet tag checked = if checked then Set.singleton tag else Set.empty

nadeTagSelector :: (MonadWidget t m) => Set.Set String -> m (Dynamic t (Set.Set String))
nadeTagSelector tags =
  elAttr "div" (toAttr $ "padding-top" =: "0.1em") $ do
    allCb <- nadeTagCheckbox True allTag
    tagCbs <- mapM f (Set.toList tags) -- [(String, Checkbox t)]
    tagCbVals <- mapM toTagSetDyn tagCbs -- [Dynamic t (Set String)]
    checkedTags <- mconcatDyn tagCbVals
    combineDyn useAllTag (_checkbox_value allCb) checkedTags
  where f tag = fmap (\r -> (tag, r)) $ nadeTagCheckbox False tag
        useAllTag allChecked tagsChecked = if allChecked then tags else tagsChecked

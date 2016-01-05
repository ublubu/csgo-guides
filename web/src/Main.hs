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
import Control.Monad.IO.Class
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

noContents :: (MonadWidget t m) => m ()
noContents = return ()

appWidget :: (MonadWidget t m) => m ()
appWidget = do
  text "map name: "
  srcText <- textInput
             def{ _textInputConfig_initialValue = "e.g. dust2"
                , _textInputConfig_attributes = constDyn ("onClick" =: "this.select()"
                                                          <> (toAttr $ S.width 100
                                                              <> S.displayInlineBlock))
                }
  let src = _textInput_value srcText
  nades_ <- getNadeInfos . fmap getMapAddress . updated $ src
  nades <- holdDyn [] nades_
  allTags <- mapDyn (mconcat . fmap (Set.fromList . _nadeTags)) nades
  checkedTags' <- dyn =<< mapDyn nadeTagSelector allTags
  checkedTags <- fmap joinDyn $ holdDyn (constDyn Set.empty) checkedTags'
  selectedNades <- combineDyn filterByTag checkedTags nades
  nadeWidgets <- mapDyn (mapM nadeInfoWidget) selectedNades
  nadeImgClicks <- eventJoin =<< (fmap (fmap leftmost) . dyn $ nadeWidgets)
  performEvent_ $ fmap (\img -> liftIO $ print img) nadeImgClicks
  nadeOverlayWidget nadeImgClicks

getMapAddress :: String -> String
getMapAddress map = "public/" ++ map ++ ".txt"

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

nadeThumb :: (MonadWidget t m) => String -> m (Event t String)
nadeThumb src = do
  (elem, _) <- elAttr' "img" ("src" =: src <> toAttr (S.width' "25%")) noContents
  return . fmap (const src) $ domEvent Click elem

nadeFullscreen :: (MonadWidget t m) => String -> m (Event t ())
nadeFullscreen src = do
  (elem, _) <- elAttr' "div" (toAttr $ "background-image" =: ("url(" ++ src ++ ")")
                             <> "background-repeat" =: "no-repeat"
                             <> "background-size" =: "contain"
                             <> "background-position" =: "center"
                             <> S.width' "100vw"
                             <> S.height' "100vh"
                             ) noContents
  return $ domEvent Click elem

nadeInfoWidget :: (MonadWidget t m) => NadeInfo -> m (Event t String)
nadeInfoWidget (NadeInfo imgs desc tags) = do
  elAttr "div" (toAttr $ "border-top" =: "2px solid black") $ do
    imgClicks <- mapM nadeThumb imgs
    elAttr "div" (toAttr $ "font-size" =: "1.5em"
                 <> "padding" =: "0.25em 0.5em 1.5em 0.5em"
                 ) $ text desc
    return . leftmost $ imgClicks

nadeTagCheckbox :: (MonadWidget t m) => Bool -> String -> m (Checkbox t)
nadeTagCheckbox startChecked tag =
  elAttr "label" (toAttr $ S.displayInlineBlock) $ do
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

overlay :: (MonadWidget t m) => m a -> m a
overlay contents =
  elAttr "div" (toAttr $ S.displayFlex
               <> S.fullWindow
               <> S.justifyContent "center"
               <> S.alignItems "center"
               <> S.posFix
               <> S.top 0) contents

nadeOverlayWidget :: (MonadWidget t m) => Event t String -> m ()
nadeOverlayWidget imgClicks = mdo
  imgDyn <- holdDyn Nothing (leftmost [fmap Just imgClicks, fmap (const Nothing) overlayClicks])
  overlayClicks <- dynWidgetEvents' maybeOverlay imgDyn
  return ()
  where maybeOverlay = maybe (return never) (overlay . nadeFullscreen)


dynWidgetEvents :: (MonadWidget t m) => Dynamic t (m (Event t a)) -> m (Event t a)
dynWidgetEvents = fmap switchPromptlyDyn . (holdDyn never =<<) . dyn

dynWidgetEvents' :: (MonadWidget t m) => (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
dynWidgetEvents' f state = dynWidgetEvents =<< mapDyn f state

eventJoin :: (MonadWidget t m) => Event t (Event t a) -> m (Event t a)
eventJoin = (return . switchPromptlyDyn) <=< holdDyn never

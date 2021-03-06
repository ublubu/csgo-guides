{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module APIForms where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators
import Data.Map (Map)
import qualified Data.Map as M

import APIClient
import Forms
import Utils

simpleApiForm :: (MonadWidget t m) => (a -> ServIO b) -> m (Dynamic t a) -> m (Event t b)
simpleApiForm fetch form = do
  val <- form
  save <- button "save"
  apiEvent fetch (tagDyn val save)

createEditForm :: (MonadWidget t m)
               => (a0 -> ServIO b)
               -> m (Dynamic t a0)
               -> (b -> (k, a1))
               -> (k -> a1 -> ServIO b)
               -> (a1 -> m (Dynamic t a1))
               -> (k -> ServIO c)
               -> m (Event t (Either c b))
createEditForm post postForm convert put putForm delete =
  widgetSequence' Right (simpleApiForm post postForm) (editForm convert put putForm delete)

editForm :: (MonadWidget t m)
         => (b -> (k, a))
         -> (k -> a -> ServIO b)
         -> (a -> m (Dynamic t a))
         -> (k -> ServIO c)
         -> b
         -> m (Event t (Either c b))
editForm convert put form delete initVal = do
  let (key, initVal') = convert initVal
  puts <- simpleApiForm (put key) (form initVal')
  dels <- apiEvent (const $ delete key) =<< button "delete"
  let updates = leftmost [fmap Left dels, fmap Right puts]
  return updates

apiEvent :: (MonadWidget t m) => (a -> ServIO b) -> Event t a -> m (Event t b)
apiEvent fetch evts = do
  responses <- performEvent (fmap doFetch evts)
  return $ fmapMaybe rightToMaybe responses
  where doFetch = liftIO . runEitherT . fetch

listItemForm :: (MonadWidget t m, Ord i)
             => (b -> (k, a))
             -> (k -> a -> ServIO b)
             -> (a -> m (Dynamic t a))
             -> (k -> ServIO c)
             -> ListItemControl i t b m
listItemForm convert put form delete listKey dVal = do
  formEvents <- dynWidgetEvents' (editForm convert put form delete) dVal
  return $ fmap f formEvents
  where f (Left _) = M.delete listKey
        f (Right val) = M.insert listKey val

addListItemForm :: (MonadWidget t m, ListKey i)
                => (a -> ServIO b)
                -> m (Dynamic t a)
                -> ListControl i t b m
addListItemForm post postForm = mdo
  formEvents <- dynWidgetEvents' (const $ simpleApiForm post postForm) postDyn
  postDyn <- holdDyn () (fmap (const ()) formEvents)
  return $ fmap f formEvents
  where f x = appendListItem x

simpleApiListForm :: forall a0 a1 b c k t m . (MonadWidget t m)
               => (a0 -> ServIO b)
               -> m (Dynamic t a0)
               -> (b -> (k, a1))
               -> (k -> a1 -> ServIO b)
               -> (a1 -> m (Dynamic t a1))
               -> (k -> ServIO c)
               -> [b]
               -> m (Dynamic t [b])
simpleApiListForm post postForm convert put putForm delete initVals =
  mapDyn M.elems =<< listForm listControl listItem initVals'
  where listControl = addListItemForm post postForm
        listItem = listItemForm convert put putForm delete
        initVals' = convertList initVals :: Map Int b

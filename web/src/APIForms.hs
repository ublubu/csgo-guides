module APIForms where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators

import APIClient
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

module APIForms where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators

import APIClient

postForm :: (MonadWidget t m) => (a -> ServIO b) -> m (Dynamic t a) -> m (Event t b)
postForm post form = do
  val <- form
  submit <- button "submit"
  responses <- performEvent (fmap doPost (tagDyn val submit))
  return $ fmapMaybe rightToMaybe responses
  where doPost = liftIO . runEitherT . post

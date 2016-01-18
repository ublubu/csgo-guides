module SignIn where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators

import qualified APIClient as API
import qualified GoogleSignIn as GSI
import API.SignIn (CookieData(..))

signInEvent :: (MonadWidget t m) => m (Event t CookieData)
signInEvent = do
  signIns <- GSI.callbackEvent
  signIns' <- performEvent (fmap toAction signIns)
  return . fmapMaybe rightToMaybe $ signIns'
  where toAction = liftIO . runEitherT . API.idtoken . Just . GSI._googleIdToken


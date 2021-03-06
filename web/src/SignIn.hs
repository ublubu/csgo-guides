module SignIn where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators

import Servant.API.ResponseHeaders (getResponse)

import qualified APIClient as API
import qualified GoogleSignIn as GSI
import API.SignIn (CookieData(..))

signInEvent :: (MonadWidget t m) => m (Event t CookieData)
signInEvent = do
  googleSignIns <- GSI.callbackEvent
  signInAttempts <- performEvent (fmap attemptSignIn googleSignIns)
  return . fmap getResponse . fmapMaybe rightToMaybe $ signInAttempts
  where attemptSignIn = liftIO . runEitherT . API.tokensignin . Just . GSI._googleIdToken

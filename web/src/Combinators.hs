{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import Reflex
import Reflex.Dom

import Control.Monad

{-
Let's assume that all widgets return a static value or Event stream.
Widgets can't do anything until they're built, so the initial value of
  any Dynamic they return would use some initial value owned by the parent.
-}

class (MonadWidget t m) => EventContainer t m a where
  ecDyn :: Dynamic t (m a) -> m a
  ecDyn = ecJoin <=< dyn

  ecDyn' :: (x -> m a) -> Dynamic t x -> m a
  ecDyn' f state = ecDyn =<< mapDyn f state

  ecNever :: m a
  ecNever = ecJoin never

  ecExtractWith :: (Functor f) => (f a -> m a) -> (x -> a) -> f x -> m a
  ecExtractWith join focus = join . fmap focus

  ecJoin :: Event t a -> m a

  ecLeftmost :: [a] -> m a

dwhen :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a
dwhen test widget =
  ecDyn' (\t -> if t then widget else ecNever) test

ewhen :: (EventContainer t m a) => Event t b -> m a -> m a
ewhen test widget = do
  test' <- holdDyn False (fmap (const True) test)
  dwhen test' widget

dif :: (EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m a -> m b -> m (a, b)
dif test true false = do
  trues <- dwhen test true
  notTest <- mapDyn not test
  falses <- dwhen notTest false
  return (trues, falses)

dif' :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a -> m a
dif' test true false = do
  (trues, falses) <- dif test true false
  ecLeftmost [trues, falses]

instance (MonadWidget t m) => EventContainer t m () where
  ecJoin = const $ return ()
  ecLeftmost = const $ return ()

instance (MonadWidget t m) => EventContainer t m (Event t a) where
  ecJoin = (return . switchPromptlyDyn) <=< holdDyn never
  ecLeftmost = return . leftmost

instance (EventContainer t m a, EventContainer t m b) => EventContainer t m (a, b) where
  ecJoin = ecTuple2 ecJoin ecJoin
  ecLeftmost = ecTuple2 ecLeftmost ecLeftmost

ecTuple2 :: (EventContainer t m a, EventContainer t m b, Functor f) => (f a -> m a) -> (f b -> m b) -> f (a, b) -> m (a, b)
ecTuple2 f g eventPairs = (,) <$> (ecExtractWith f) fst eventPairs <*> (ecExtractWith g) snd eventPairs

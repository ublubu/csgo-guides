module Utils where

import Control.Monad
import Reflex
import Reflex.Dom

noContents :: (MonadWidget t m) => m ()
noContents = return ()

dynWidgetDyn :: (MonadWidget t m) => a -> Dynamic t (m (Dynamic t a)) -> m (Dynamic t a)
dynWidgetDyn val = fmap joinDyn . (holdDyn (constDyn val) =<<) . dyn

dynWidgetDyn' :: (MonadWidget t m) => b -> (a -> m (Dynamic t b)) -> Dynamic t a -> m (Dynamic t b)
dynWidgetDyn' val f state = dynWidgetDyn val =<< mapDyn f state

dynWidgetEvents :: (MonadWidget t m) => Dynamic t (m (Event t a)) -> m (Event t a)
dynWidgetEvents = fmap switchPromptlyDyn . (holdDyn never =<<) . dyn

dynWidgetEvents' :: (MonadWidget t m) => (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
dynWidgetEvents' f state = dynWidgetEvents =<< mapDyn f state

eventJoin :: (MonadWidget t m) => Event t (Event t a) -> m (Event t a)
eventJoin = (return . switchPromptlyDyn) <=< holdDyn never

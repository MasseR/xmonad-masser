module XMonad.Configurable (Configurable, EndoM(..), configure) where

import Control.Monad.Reader

newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance Monad m => Semigroup (EndoM m a) where
  EndoM f <> EndoM g = EndoM (f <=< g)

instance Monad m => Monoid (EndoM m a) where
  mempty = EndoM pure

type Configurable a = EndoM (Reader a) a

configure :: Configurable a -> a
configure (EndoM f) = fix (\self -> runReader (f self) self)

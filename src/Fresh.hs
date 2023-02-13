{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fresh
  ( Fresh (..),
    GenFresh (..),
    FreshT,
    runFreshT,
  )
where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, state)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT)
import Numeric.Natural (Natural)

class Monad m => Fresh m where
  fresh :: GenFresh a => m a

instance Fresh m => Fresh (ExceptT e m) where
  fresh = lift fresh

instance Fresh m => Fresh (MaybeT m) where
  fresh = lift fresh

instance Fresh m => Fresh (ReaderT r m) where
  fresh = lift fresh

instance Fresh m => Fresh (StateT s m) where
  fresh = lift fresh

instance (Monoid w, Fresh m) => Fresh (WriterT w m) where
  fresh = lift fresh

newtype FreshT m a = MkFreshT (StateT Natural m a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadLogger, MonadIO)

instance MonadTrans FreshT where
  lift = MkFreshT . lift

instance Monad m => Fresh (FreshT m) where
  fresh = MkFreshT $ state f
    where
      f n = (fromFreshNatural n, succ n)

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (MkFreshT m) = evalStateT m 0

class GenFresh a where
  fromFreshNatural :: Natural -> a

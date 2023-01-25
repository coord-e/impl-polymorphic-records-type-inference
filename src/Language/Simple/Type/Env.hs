{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Env
  ( EnvT,
    runEnvT,
    HasTypeEnv (..),
    withLocalVar,
    TermVarType (..),
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT (..), ask, asks, local, runReaderT)
import Control.Monad.State (StateT (..), evalStateT, state)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (insert, lookup)
import Data.HashSet (HashSet)
import Fresh (Fresh (..), GenFresh (..))
import GHC.Generics (Generic)
import Language.Simple.Syntax (DataCtor (..), Monotype (..), TermVar, TypeCtor (..), TypeScheme (..))
import Language.Simple.Type.Constraint (UniVar, fuv)
import Numeric.Natural (Natural)

data TermVarType
  = TypeScheme (TypeScheme UniVar)
  | Monotype (Monotype UniVar)
  deriving (Generic)

class Monad m => HasTypeEnv m where
  lookupTermVar :: TermVar -> m (Maybe TermVarType)
  withTermVar :: TermVar -> TermVarType -> m a -> m a
  lookupDataCtor :: DataCtor -> m (Maybe (TypeScheme UniVar))
  envFuv :: m (HashSet UniVar)

withLocalVar :: HasTypeEnv m => TermVar -> Monotype UniVar -> m a -> m a
withLocalVar v t = withTermVar v (Monotype t)

data Env = Env
  { termVars :: HashMap TermVar TermVarType
  }

newtype EnvT m a = MkEnvT (ReaderT Env (StateT Natural m) a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadLogger)

instance Monad m => HasTypeEnv (EnvT m) where
  lookupTermVar x = MkEnvT . asks $ HashMap.lookup x . termVars
  withTermVar x s (MkEnvT a) = MkEnvT $ local f a
    where
      f e@Env {termVars} = e {termVars = HashMap.insert x s termVars}
  lookupDataCtor (NamedDataCtor "True") = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Bool") mempty}
  lookupDataCtor (NamedDataCtor "False") = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Bool") mempty}
  lookupDataCtor (IntegerDataCtor _) = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Int") mempty}
  lookupDataCtor _ = pure $ Nothing
  envFuv = MkEnvT $ foldMap f . termVars <$> ask
    where
      f (TypeScheme s) = fuv $ monotype s
      f (Monotype t) = fuv t

instance (Monoid w, HasTypeEnv m) => HasTypeEnv (WriterT w m) where
  lookupTermVar x = lift $ lookupTermVar x
  withTermVar x s m = mapWriterT (withTermVar x s) m
  lookupDataCtor x = lift $ lookupDataCtor x
  envFuv = lift envFuv

instance Monad m => Fresh (EnvT m) where
  fresh = MkEnvT $ state f
    where
      f n = (fromFreshNatural n, succ n)

runEnvT ::
  Monad m =>
  HashMap TermVar TermVarType ->
  EnvT m a ->
  m a
runEnvT termVars (MkEnvT a) = evalStateT (runReaderT a initEnv) 0
  where
    initEnv = Env {termVars}

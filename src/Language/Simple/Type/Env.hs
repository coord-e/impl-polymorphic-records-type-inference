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
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT (..), asks, local, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (insert, lookup)
import Data.HashSet (HashSet)
import Fresh (Fresh (..))
import Language.Simple.Syntax
  ( DataCtor (..),
    Monotype (..),
    TermVar,
    TypeCtor (..),
    TypeScheme (..),
    monoTypeScheme,
  )
import Language.Simple.Type.Subst (Subst, Substitutable)
import qualified Language.Simple.Type.Subst as Subst (substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)

class Monad m => HasTypeEnv m where
  lookupTermVar :: TermVar -> m (Maybe (TypeScheme UniVar))
  withTermVar :: TermVar -> TypeScheme UniVar -> m a -> m a
  lookupDataCtor :: DataCtor -> m (Maybe (TypeScheme UniVar))
  envFuv :: m (HashSet UniVar)
  withSubst :: Substitutable a (TypeScheme UniVar) => Subst a -> m b -> m b

withLocalVar :: HasTypeEnv m => TermVar -> Monotype UniVar -> m a -> m a
withLocalVar v t = withTermVar v $ monoTypeScheme t

newtype TypeEnv = TypeEnv
  { termVars :: HashMap TermVar (TypeScheme UniVar)
  }

newtype EnvT m a = MkEnvT (ReaderT TypeEnv m a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadLogger, Fresh)

instance Monad m => HasTypeEnv (EnvT m) where
  lookupTermVar x = MkEnvT . asks $ HashMap.lookup x . termVars
  withTermVar x s (MkEnvT a) = MkEnvT $ local f a
    where
      f e@TypeEnv {termVars} = e {termVars = HashMap.insert x s termVars}
  lookupDataCtor (NamedDataCtor "True") = pure . Just . monoTypeScheme $ ApplyType (NamedTypeCtor "Bool") mempty
  lookupDataCtor (NamedDataCtor "False") = pure . Just . monoTypeScheme $ ApplyType (NamedTypeCtor "Bool") mempty
  lookupDataCtor (IntegerDataCtor _) = pure . Just . monoTypeScheme $ ApplyType (NamedTypeCtor "Int") mempty
  lookupDataCtor _ = pure Nothing
  envFuv = MkEnvT $ asks (foldMap (fuv . monotype) . termVars)
  withSubst u (MkEnvT a) = MkEnvT $ local f a
    where
      f e@TypeEnv {termVars} = e {termVars = fmap (Subst.substitute u) termVars}

instance (Monoid w, HasTypeEnv m) => HasTypeEnv (WriterT w m) where
  lookupTermVar x = lift $ lookupTermVar x
  withTermVar x s = mapWriterT (withTermVar x s)
  lookupDataCtor x = lift $ lookupDataCtor x
  envFuv = lift envFuv
  withSubst u = mapWriterT (withSubst u)

runEnvT ::
  Monad m =>
  HashMap TermVar (TypeScheme UniVar) ->
  EnvT m a ->
  m a
runEnvT termVars (MkEnvT a) = runReaderT a initTypeEnv
  where
    initTypeEnv = TypeEnv {termVars}

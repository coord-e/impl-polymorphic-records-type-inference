{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Env
  ( EnvT,
    runEnvT,
    HasTypeEnv (..),
    HasKindEnv (..),
    withLocalVar,
  )
where

import Control.Exception (throw)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT (..), asks, local, runReaderT)
import Control.Monad.State (StateT (..), evalStateT, gets, state)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (insert, lookup, member)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Fresh (Fresh (..))
import Language.Simple.Syntax
  ( DataCtor (..),
    Kind,
    Monotype (..),
    TermVar,
    TypeCtor (..),
    TypeScheme (..),
  )
import Language.Simple.Type.Error (TypeException (..))
import Language.Simple.Type.Subst (Unifier)
import qualified Language.Simple.Type.Subst as Subst (substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)

class Monad m => HasTypeEnv m where
  lookupTermVar :: TermVar -> m (Maybe (TypeScheme UniVar))
  withTermVar :: TermVar -> TypeScheme UniVar -> m a -> m a
  lookupDataCtor :: DataCtor -> m (Maybe (TypeScheme UniVar))
  envFuv :: m (HashSet UniVar)
  withUnifier :: Unifier -> m a -> m a

withLocalVar :: HasTypeEnv m => TermVar -> Monotype UniVar -> m a -> m a
withLocalVar v t = withTermVar v ForallTypeScheme {vars = mempty, monotype = t}

class Monad m => HasKindEnv m where
  getUniVarKind :: UniVar -> m (Kind UniVar)
  setUniVarKind :: UniVar -> Kind UniVar -> m ()
  newUniVar :: Kind UniVar -> m UniVar
  substKindEnv :: Substitutable a (Kind UniVar) => Subst a -> m ()

newtype TypeEnv = TypeEnv
  { termVars :: HashMap TermVar (TypeScheme UniVar)
  }

newtype KindEnv = KindEnv
  { uniVars :: HashMap UniVar (Kind UniVar)
  }

newtype EnvT m a = MkEnvT (ReaderT TypeEnv (StateT KindEnv m) a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadLogger, Fresh)

instance Monad m => HasTypeEnv (EnvT m) where
  lookupTermVar x = MkEnvT . asks $ HashMap.lookup x . termVars
  withTermVar x s (MkEnvT a) = MkEnvT $ local f a
    where
      f e@TypeEnv {termVars} = e {termVars = HashMap.insert x s termVars}
  lookupDataCtor (NamedDataCtor "True") = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Bool") mempty}
  lookupDataCtor (NamedDataCtor "False") = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Bool") mempty}
  lookupDataCtor (IntegerDataCtor _) = pure $ Just ForallTypeScheme {vars = mempty, monotype = ApplyType (NamedTypeCtor "Int") mempty}
  lookupDataCtor _ = pure Nothing
  envFuv = MkEnvT $ asks (foldMap (fuv . monotype) . termVars)
  withUnifier u (MkEnvT a) = MkEnvT $ local f a
    where
      f e@TypeEnv {termVars} = e {termVars = fmap (Subst.substitute u) termVars}

instance (Monoid w, HasTypeEnv m) => HasTypeEnv (WriterT w m) where
  lookupTermVar x = lift $ lookupTermVar x
  withTermVar x s = mapWriterT (withTermVar x s)
  lookupDataCtor x = lift $ lookupDataCtor x
  envFuv = lift envFuv
  withUnifier u = mapWriterT (withUnifier u)

instance (Fresh m, Monad m) => HasKindEnv (EnvT m) where
  getUniVarKind u = MkEnvT . gets $ f . HashMap.lookup u . uniVars
    where
      f = fromMaybe (throw $ UniVarWithoutKindException u)
  setUniVarKind u k = MkEnvT $ modify f
    where
      f KindEnv {uniVars}
        | HashMap.member u uniVars = KindEnv {uniVars = HashMap.insert u k uniVars}
        | otherwise = throw $ UniVarWithoutKindException u
  newUniVar k = MkEnvT $ fresh >>= state . f
    where
      f u KindEnv {uniVars} =
        ( u,
          KindEnv
            { uniVars = HashMap.insert u k uniVars
            }
        )
  substKindEnv s = MkEnvT $ modify f
    where
      f KindEnv {uniVars} = KindEnv {uniVars = fmap (Subst.substitute s) uniVars}

instance (Monoid w, HasKindEnv m) => HasKindEnv (WriterT w m) where
  getUniVarKind u = lift $ getUniVarKind u
  setUniVarKind u k = lift $ setUniVarKind u k
  newUniVar k = lift $ newUniVar k
  substKindEnv s = lift $ substKindEnv s

instance HasKindEnv m => HasKindEnv (StateT s m) where
  getUniVarKind u = lift $ getUniVarKind u
  setUniVarKind u k = lift $ setUniVarKind u k
  newUniVar k = lift $ newUniVar k
  substKindEnv s = lift $ substKindEnv s

runEnvT ::
  Monad m =>
  HashMap TermVar (TypeScheme UniVar) ->
  EnvT m a ->
  m a
runEnvT termVars (MkEnvT a) = evalStateT (runReaderT a initTypeEnv) initKindEnv
  where
    initTypeEnv = TypeEnv {termVars}
    initKindEnv = KindEnv {uniVars = mempty}

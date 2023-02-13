{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type
  ( typeExpr,
    TypeError (..),
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Writer (MonadWriter (..), listen, runWriterT)
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as HashMap (singleton)
import qualified Data.HashSet as HashSet (difference)
import qualified Data.Vector as Vector (fromList)
import Fresh (Fresh (..), runFreshT)
import Language.Simple.Syntax (Expr (..), Kind (..), Monotype (..), TypeScheme (..), TypeVar, functionType)
import Language.Simple.Type.Constraint (Constraint (..), solveConstraints)
import Language.Simple.Type.Env (HasKindEnv (..), HasTypeEnv (..), runEnvT, withLocalVar)
import Language.Simple.Type.Error (TypeError (..))
import Language.Simple.Type.Subst (Subst)
import qualified Language.Simple.Type.Subst as Subst (compose, empty, lookup, singleton, substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)
import Util (orThrow, orThrowM, showPretty)

generateConstraints ::
  ( HasTypeEnv m,
    HasKindEnv m,
    Fresh m,
    MonadWriter [Constraint] m,
    MonadLogger m,
    MonadError TypeError m
  ) =>
  Expr ->
  m (Monotype UniVar)
generateConstraints (CtorExpr k) = do
  s <- lookupDataCtor k `orThrowM` UnboundDataCtor k
  instantiateTypeScheme s
generateConstraints (VarExpr x) = do
  s <- lookupTermVar x `orThrowM` UnboundTermVar x
  instantiateTypeScheme s
generateConstraints (LambdaExpr x e) = do
  a <- UniType <$> newUniVar TypeKind
  t <- withLocalVar x a $ generateConstraints e
  pure $ functionType a t
generateConstraints (ApplyExpr e1 e2) = do
  t1 <- generateConstraints e1
  t2 <- generateConstraints e2
  a <- UniType <$> newUniVar TypeKind
  tell [EqualityConstraint t1 (functionType t2 a)]
  pure a
generateConstraints (LetExpr x e1 e2) = do
  (t1, cs) <- listen $ generateConstraints e1
  u <- solveConstraints cs
  s <- withSubst u $ generalize (Subst.substitute u t1)
  logInfoN $ showPretty x <> " :: " <> showPretty s
  withTermVar x s $ generateConstraints e2
generateConstraints (RecordExpr fs) = do
  fs' <- traverse generateConstraints fs
  pure $ RecordType fs'
generateConstraints (UpdateExpr fs e) = do
  t <- generateConstraints e
  fs' <- forM fs $ \e2 -> do
    t2 <- generateConstraints e2
    a <- UniType <$> newUniVar TypeKind
    tell [EqualityConstraint a t2]
    pure a
  a <- UniType <$> newUniVar (RecordKind fs')
  tell [EqualityConstraint a t]
  pure a
generateConstraints (MemberExpr e l) = do
  t <- generateConstraints e
  a1 <- UniType <$> newUniVar TypeKind
  a2 <- UniType <$> newUniVar (RecordKind $ HashMap.singleton l a1)
  tell [EqualityConstraint a2 t]
  pure a1

generalize ::
  ( Fresh m,
    MonadLogger m,
    HasKindEnv m,
    HasTypeEnv m
  ) =>
  Monotype UniVar ->
  m (TypeScheme UniVar)
generalize t = do
  as <- HashSet.difference (fuv t) <$> envFuv
  (s, vs) <- foldrM go (Subst.empty, []) as
  let vs' = fmap (f s) vs
  pure ForallTypeScheme {vars = Vector.fromList vs', monotype = Subst.substitute s t}
  where
    go a (s, vs) = do
      v <- fresh
      let s' = Subst.singleton a (VarType v)
      k <- getUniVarKind a
      pure (Subst.compose s s', (v, k) : vs)
    f s (v, k) = (v, Subst.substitute s k)

type Instantiator = Subst TypeVar

instantiateMonotype :: (MonadError TypeError m) => Instantiator -> Monotype UniVar -> m (Monotype UniVar)
instantiateMonotype s (VarType v) = Subst.lookup v s `orThrow` UnboundTypeVar v
instantiateMonotype s (ApplyType k ts) = ApplyType k <$> traverse (instantiateMonotype s) ts
instantiateMonotype s (RecordType fs) = RecordType <$> traverse (instantiateMonotype s) fs
instantiateMonotype _ (UniType v) = pure $ UniType v

instantiateTypeScheme ::
  ( Fresh m,
    MonadError TypeError m,
    HasKindEnv m
  ) =>
  TypeScheme UniVar ->
  m (Monotype UniVar)
instantiateTypeScheme ForallTypeScheme {vars, monotype} = do
  s <- foldlM go Subst.empty vars
  substKindEnv s
  instantiateMonotype s monotype
  where
    go s (v, k) = do
      a <- newUniVar k
      let s' = Subst.singleton v (UniType a)
      pure $ Subst.compose s' s

typeExpr :: (MonadError TypeError m, MonadLogger m) => Expr -> m ()
typeExpr e = runFreshT $ do
  t <- runEnvT mempty $ do
    (t, cs) <- runWriterT $ generateConstraints e
    s <- solveConstraints cs
    pure $ Subst.substitute s t
  logInfoN $ showPretty t
  pure ()

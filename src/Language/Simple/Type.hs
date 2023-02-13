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
import qualified Data.HashSet as HashSet (difference, member)
import qualified Data.Vector as Vector (fromList, zip)
import Fresh (Fresh (..), runFreshT)
import Language.Simple.Syntax (Expr (..), Kind (..), Monotype (..), TypeScheme (..), TypeVar, functionType)
import Language.Simple.Type.Constraint (Constraint (..))
import Language.Simple.Type.Env (HasKindEnv (..), HasTypeEnv (..), runEnvT, withLocalVar)
import Language.Simple.Type.Error (TypeError (..))
import Language.Simple.Type.Subst (Subst, Unifier)
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
  s <- withUnifier u $ generalize (Subst.substitute u t1)
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

solveConstraints :: (MonadError TypeError m, MonadLogger m) => [Constraint] -> m Unifier
solveConstraints = foldrM go Subst.empty
  where
    go (EqualityConstraint t1 t2) s1 = do
      s2 <- unifyLog (Subst.substitute s1 t1) (Subst.substitute s1 t2)
      pure $ Subst.compose s2 s1
    unifyLog t1 t2 = do
      logInfoN $ "unify: " <> showPretty (EqualityConstraint t1 t2)
      s <- unify t1 t2
      logInfoN $ "got: " <> showPretty s
      pure s
    unify (VarType v1) (VarType v2) | v1 == v2 = pure Subst.empty
    unify (UniType u) t = unifyVar u t
    unify t (UniType u) = unifyVar u t
    unify (ApplyType k1 ts1) (ApplyType k2 ts2)
      | k1 == k2 && length ts1 == length ts2 = unifyAll ts1 ts2
    unify t1 t2 = throwError $ MismatchedType t1 t2
    unifyVar u t
      | HashSet.member u (fuv t) = throwError $ OccurCheck (UniType u) t
      | otherwise = pure $ Subst.singleton u t
    unifyAll xs ys = foldrM go Subst.empty . fmap f $ Vector.zip xs ys
      where
        f (x, y) = EqualityConstraint x y

typeExpr :: (MonadError TypeError m, MonadLogger m) => Expr -> m ()
typeExpr e = runFreshT $ do
  (t, cs) <- runEnvT mempty . runWriterT $ generateConstraints e
  s <- solveConstraints cs
  logInfoN . showPretty $ Subst.substitute s t
  pure ()

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Simple.Type
  ( typeExpr,
    TypeError (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.Foldable (foldrM)
import qualified Data.HashSet as HashSet (difference, member)
import Data.Text (pack)
import qualified Data.Vector as Vector (fromList, zip)
import Fresh (Fresh (..))
import Language.Simple.Syntax (Expr (..), Monotype (..), TypeScheme (..), TypeVar, functionType)
import Language.Simple.Type.Constraint (Constraint (..), UniVar, fuv)
import Language.Simple.Type.Env (HasTypeEnv (..), TermVarType (..), runEnvT, withLocalVar)
import Language.Simple.Type.Error (TypeError (..))
import Language.Simple.Type.Subst (Subst, Unifier)
import qualified Language.Simple.Type.Subst as Subst (compose, empty, fromBinders, lookup, singleton, substitute)
import Util (orThrow, orThrowM)

generateConstraints :: (HasTypeEnv m, Fresh m, MonadWriter [Constraint] m, MonadLogger m, MonadError TypeError m) => Expr -> m (Monotype UniVar)
generateConstraints (CtorExpr k) = do
  s <- lookupDataCtor k `orThrowM` UnboundDataCtor k
  instantiateTypeScheme s
generateConstraints (VarExpr x) = do
  q <- lookupTermVar x `orThrowM` UnboundTermVar x
  case q of
    TypeScheme s -> instantiateTypeScheme s
    Monotype t -> pure t
generateConstraints (LambdaExpr x e) = do
  a <- UniType <$> fresh
  t <- withLocalVar x a $ generateConstraints e
  pure $ functionType a t
generateConstraints (ApplyExpr e1 e2) = do
  t1 <- generateConstraints e1
  t2 <- generateConstraints e2
  a <- UniType <$> fresh
  tell $ [EqualityConstraint t1 (functionType t2 a)]
  pure a
generateConstraints (LetExpr x e1 e2) = do
  (t1, cs) <- runWriterT $ generateConstraints e1
  u <- solveConstraints cs
  s <- generalize $ Subst.substitute u t1
  t2 <- withTermVar x (TypeScheme s) $ generateConstraints e2
  pure t2

generalize :: (Fresh m, MonadLogger m, HasTypeEnv m) => Monotype UniVar -> m (TypeScheme UniVar)
generalize t = do
  as <- HashSet.difference (fuv t) <$> envFuv
  (s, vs) <- foldrM go (Subst.empty, []) as
  pure ForallTypeScheme {vars = Vector.fromList vs, monotype = Subst.substitute s t}
  where
    go a (s, vs) = do
      v <- fresh
      let s' = Subst.singleton a (VarType v)
      pure (Subst.compose s s', v : vs)

type Instantiator = Subst TypeVar

instantiateMonotype :: (MonadError TypeError m) => Instantiator -> Monotype UniVar -> m (Monotype UniVar)
instantiateMonotype s (VarType v) = Subst.lookup v s `orThrow` UnboundTypeVar v
instantiateMonotype s (ApplyType k ts) = ApplyType k <$> traverse (instantiateMonotype s) ts
instantiateMonotype _ (UniType v) = pure $ UniType v

instantiateTypeScheme ::
  ( Fresh m,
    MonadError TypeError m
  ) =>
  TypeScheme UniVar ->
  m (Monotype UniVar)
instantiateTypeScheme ForallTypeScheme {vars, monotype} = do
  s <- Subst.fromBinders vars
  t <- instantiateMonotype s monotype
  pure t

solveConstraints :: (MonadError TypeError m, MonadLogger m) => [Constraint] -> m Unifier
solveConstraints = foldrM go Subst.empty
  where
    go (EqualityConstraint t1 t2) s1 = do
      logInfoN . pack $ show t1 <> " ~ " <> show t2
      s2 <- unify (Subst.substitute s1 t1) (Subst.substitute s1 t2)
      pure $ Subst.compose s2 s1
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
typeExpr e = do
  (t, cs) <- runEnvT mempty . runWriterT $ generateConstraints e
  s <- solveConstraints cs
  logInfoN . pack . show $ Subst.substitute s t
  pure ()

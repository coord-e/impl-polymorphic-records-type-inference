{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type
  ( typeExpr,
    TypeError (..),
  )
where

import Control.Exception (throw)
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer (MonadWriter (..), listen, runWriterT)
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as HashMap (singleton)
import qualified Data.HashSet as HashSet (difference)
import qualified Data.Vector as Vector (fromList, toList)
import Fresh (Fresh (..), runFreshT)
import Language.Simple.Syntax (Expr (..), Monotype (..), RecordConstraint (..), TypeScheme (..), TypeVar, functionType)
import Language.Simple.Type.Constraint (Constraint (..), EqualityConstraint (..), solveConstraints)
import Language.Simple.Type.Env (HasTypeEnv (..), runEnvT, withLocalVar)
import Language.Simple.Type.Error (TypeError (..), TypeException (..))
import Language.Simple.Type.Subst (Subst)
import qualified Language.Simple.Type.Subst as Subst (compose, empty, lookup, singleton, substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)
import Prettyprinter (pretty, (<+>))
import Util (logInfoDoc, orThrow, orThrowM)

generateConstraints ::
  ( HasTypeEnv m,
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
  a <- UniType <$> fresh
  t <- withLocalVar x a $ generateConstraints e
  pure $ functionType a t
generateConstraints (ApplyExpr e1 e2) = do
  t1 <- generateConstraints e1
  t2 <- generateConstraints e2
  a <- UniType <$> fresh
  tell [EqualityConstraint (t1 :~: functionType t2 a)]
  pure a
generateConstraints (LetExpr x e1 e2) = do
  (t1, cs) <- listen $ generateConstraints e1
  (u, residual) <- solveConstraints cs
  s <- withSubst u $ generalize residual (Subst.substitute u t1)
  logInfoDoc $ pretty x <+> "::" <+> pretty s
  withTermVar x s $ generateConstraints e2
generateConstraints (RecordExpr fs) = do
  fs' <- traverse generateConstraints fs
  pure $ RecordType fs'
generateConstraints (UpdateExpr fs e) = do
  t <- generateConstraints e
  fs' <- forM fs $ \e2 -> do
    t2 <- generateConstraints e2
    a <- UniType <$> fresh
    tell [EqualityConstraint (a :~: t2)]
    pure a
  a <- fresh
  tell [RecordConstraint (fs' :<: a), EqualityConstraint (UniType a :~: t)]
  pure $ UniType a
generateConstraints (MemberExpr e l) = do
  t <- generateConstraints e
  a1 <- UniType <$> fresh
  a2 <- fresh
  tell
    [ RecordConstraint (HashMap.singleton l a1 :<: a2),
      EqualityConstraint (UniType a2 :~: t)
    ]
  pure a1

generalize ::
  ( Fresh m,
    MonadLogger m,
    HasTypeEnv m
  ) =>
  [RecordConstraint UniVar UniVar] ->
  Monotype UniVar ->
  m (TypeScheme UniVar)
generalize residual t = do
  as <- HashSet.difference (fuv t <> foldMap fuvConstraint residual) <$> envFuv
  (s, vs) <- foldrM go (Subst.empty, []) as
  let constraints = Vector.fromList $ fmap (genConstraint s) residual
  pure ForallTypeScheme {vars = Vector.fromList vs, constraints, monotype = Subst.substitute s t}
  where
    go a (s, vs) = do
      v <- fresh
      let s' = Subst.singleton a (VarType v)
      pure (Subst.compose s s', v : vs)
    fuvConstraint (fs :<: _) = foldMap fuv fs
    genConstraint s c@(fs :<: u) = case Subst.lookup u s of
      Just (VarType v) -> fmap (Subst.substitute s) fs :<: v
      _ -> throw $ FreeUniVarInConstraintException c

type Instantiator = Subst TypeVar

instantiateMonotype :: (MonadError TypeError m) => Instantiator -> Monotype UniVar -> m (Monotype UniVar)
instantiateMonotype s (VarType v) = Subst.lookup v s `orThrow` UnboundTypeVar v
instantiateMonotype s (ApplyType k ts) = ApplyType k <$> traverse (instantiateMonotype s) ts
instantiateMonotype s (RecordType fs) = RecordType <$> traverse (instantiateMonotype s) fs
instantiateMonotype _ (UniType v) = pure $ UniType v

instantiateTypeScheme ::
  ( Fresh m,
    MonadError TypeError m,
    MonadWriter [Constraint] m
  ) =>
  TypeScheme UniVar ->
  m (Monotype UniVar)
instantiateTypeScheme ForallTypeScheme {vars, constraints, monotype} = do
  s <- foldlM go Subst.empty vars
  tell . Vector.toList $ fmap (RecordConstraint . instConstraint s) constraints
  instantiateMonotype s monotype
  where
    go s v = do
      a <- fresh
      let s' = Subst.singleton v (UniType a)
      pure $ Subst.compose s' s
    instConstraint s c@(fs :<: v) = case Subst.lookup v s of
      Just (UniType u) -> fmap (Subst.substitute s) fs :<: u
      _ -> throw $ FreeTypeVarInConstraintException c

typeExpr :: (MonadError TypeError m, MonadLogger m) => Expr -> m ()
typeExpr e = runFreshT $ do
  (t, cs) <- runEnvT mempty . runWriterT $ generateConstraints e
  (s, _) <- solveConstraints cs
  logInfoDoc . pretty $ Subst.substitute s t
  pure ()

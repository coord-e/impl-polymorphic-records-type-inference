{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Constraint
  ( Constraint (..),
    solveConstraints,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.State (MonadState, execStateT, get, modify)
import qualified Data.HashMap.Strict as HashMap
  ( difference,
    elems,
    intersectionWith,
    null,
    union,
  )
import qualified Data.HashSet as HashSet (member)
import qualified Data.Vector as Vector (toList, zipWith)
import GHC.Generics (Generic)
import Language.Simple.Syntax (Kind (..), Monotype (..))
import Language.Simple.Type.Env (HasKindEnv (..))
import Language.Simple.Type.Error (TypeError (..))
import Language.Simple.Type.Subst (Subst, Substitutable (..))
import qualified Language.Simple.Type.Subst as Subst (compose, empty, singleton, substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)
import Prettyprinter (Pretty (..), group, (<+>))
import Util (logInfoDoc)

data Constraint = EqualityConstraint (Monotype UniVar) (Monotype UniVar)
  deriving (Generic)

instance Substitutable UniVar Constraint where
  substitute s (EqualityConstraint t1 t2) = EqualityConstraint (Subst.substitute s t1) (Subst.substitute s t2)

instance Pretty Constraint where
  pretty (EqualityConstraint t1 t2) = pretty t1 <+> "~" <+> pretty t2

solveConstraints ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m
  ) =>
  [Constraint] ->
  m (Subst UniVar)
solveConstraints cs = execStateT (go cs) Subst.empty
  where
    go [] = pure ()
    go (h : t) = do
      cs' <- solveConstraint h
      logInfoDoc $ pretty h <+> "=>" <+> group (pretty cs')
      s <- get
      go $ fmap (Subst.substitute s) (cs' ++ t)

solveConstraint ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m,
    MonadState (Subst UniVar) m
  ) =>
  Constraint ->
  m [Constraint]
solveConstraint (EqualityConstraint (UniType u) t) = do
  k <- getUniVarKind u
  unifyUniVarWithLogging u k t
solveConstraint (EqualityConstraint t (UniType u)) = do
  k <- getUniVarKind u
  unifyUniVarWithLogging u k t
solveConstraint (EqualityConstraint (ApplyType k1 ts1) (ApplyType k2 ts2))
  | k1 == k2 && length ts1 == length ts2 =
      pure . Vector.toList $ Vector.zipWith EqualityConstraint ts1 ts2
solveConstraint (EqualityConstraint (RecordType fs1) (RecordType fs2))
  | equalLabels = pure $ HashMap.elems intersection
  where
    intersection = HashMap.intersectionWith EqualityConstraint fs1 fs2
    equalLabels =
      HashMap.null (fs1 `HashMap.difference` intersection)
        && HashMap.null (fs2 `HashMap.difference` intersection)
solveConstraint (EqualityConstraint t1 t2)
  | t1 == t2 = pure []
  | otherwise = throwError $ MismatchedType t1 t2

unifyUniVarWithLogging ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m,
    MonadState (Subst UniVar) m
  ) =>
  UniVar ->
  Kind UniVar ->
  Monotype UniVar ->
  m [Constraint]
unifyUniVarWithLogging u1 k1 t2@(UniType u2) = do
  k2 <- getUniVarKind u2
  logInfoDoc $ pretty u1 <> "::" <> pretty k1 <+> "~" <+> pretty u2 <> "::" <> pretty k2
  unifyUniVar u1 k1 t2
unifyUniVarWithLogging u1 k1 t2 = do
  logInfoDoc $ pretty u1 <> "::" <> pretty k1 <+> "~" <+> pretty t2
  unifyUniVar u1 k1 t2

unifyUniVar ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m,
    MonadState (Subst UniVar) m
  ) =>
  UniVar ->
  Kind UniVar ->
  Monotype UniVar ->
  m [Constraint]
unifyUniVar u1 k1 (UniType u2) = do
  k2 <- getUniVarKind u2
  unifyUniVars u1 k1 u2 k2
unifyUniVar u k@(RecordKind fs1) t@(RecordType fs2)
  | HashMap.null (fs1 `HashMap.difference` intersection) = do
      unifyUniVarWithoutKind u t
      pure $ HashMap.elems intersection
  | otherwise = throwError $ MismatchedKind u k (RecordKind fs2)
  where
    intersection = HashMap.intersectionWith EqualityConstraint fs1 fs2
unifyUniVar u TypeKind t = [] <$ unifyUniVarWithoutKind u t
unifyUniVar u k@(RecordKind _) _ = throwError $ MismatchedKind u TypeKind k

unifyUniVars ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m,
    MonadState (Subst UniVar) m
  ) =>
  UniVar ->
  Kind UniVar ->
  UniVar ->
  Kind UniVar ->
  m [Constraint]
unifyUniVars u1 (RecordKind _) u2 TypeKind = [] <$ unifyUniVarWithoutKind u2 (UniType u1)
unifyUniVars u1 TypeKind u2 _ = [] <$ unifyUniVarWithoutKind u1 (UniType u2)
unifyUniVars u1 (RecordKind fs1) u2 (RecordKind fs2) = do
  setUniVarKind u2 (RecordKind union)
  unifyUniVarWithoutKind u1 (UniType u2)
  pure $ HashMap.elems intersection
  where
    intersection = HashMap.intersectionWith EqualityConstraint fs1 fs2
    union = HashMap.union fs1 fs2

unifyUniVarWithoutKind ::
  ( MonadError TypeError m,
    MonadLogger m,
    HasKindEnv m,
    MonadState (Subst UniVar) m
  ) =>
  UniVar ->
  Monotype UniVar ->
  m ()
unifyUniVarWithoutKind u1 (UniType u2) | u1 == u2 = pure ()
unifyUniVarWithoutKind u t
  | HashSet.member u (fuv t) = throwError $ OccurCheck (UniType u) t
  | otherwise = do
      modify $ Subst.compose s
      substKindEnv s
      logInfoDoc $ pretty s
  where
    s = Subst.singleton u t

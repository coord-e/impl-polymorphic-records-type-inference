{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Constraint
  ( Constraint (..),
    EqualityConstraint (..),
    solveConstraints,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.State (modify, runStateT)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import Data.Foldable (foldrM)
import qualified Data.HashMap.Strict as HashMap
  ( difference,
    elems,
    intersectionWith,
    keysSet,
    null,
    union,
  )
import qualified Data.HashSet as HashSet (member)
import qualified Data.Map.Strict as Map (insert, lookup, toList)
import qualified Data.Vector as Vector (toList, zipWith)
import GHC.Generics (Generic)
import Language.Simple.Syntax (Monotype (..), RecordConstraint (..))
import Language.Simple.Type.Error (TypeError (..))
import Language.Simple.Type.Subst (Subst, Substitutable (..))
import qualified Language.Simple.Type.Subst as Subst (compose, empty, singleton, substitute)
import Language.Simple.Type.UniVar (UniVar, fuv)
import Prettyprinter (Pretty (..), group, line, vsep, (<+>))
import Util (logInfoDoc, mapMaybeM, untilM)

data EqualityConstraint = Monotype UniVar :~: Monotype UniVar
  deriving (Generic)

data Constraint
  = EqualityConstraint EqualityConstraint
  | RecordConstraint (RecordConstraint UniVar UniVar)
  deriving (Generic)

instance Substitutable UniVar EqualityConstraint where
  substitute s (t1 :~: t2) = Subst.substitute s t1 :~: Subst.substitute s t2

instance Pretty EqualityConstraint where
  pretty (t1 :~: t2) = pretty t1 <+> "~" <+> pretty t2

instance Pretty Constraint where
  pretty (EqualityConstraint c) = pretty c
  pretty (RecordConstraint c) = pretty c

solveConstraints ::
  ( MonadError TypeError m,
    MonadLogger m
  ) =>
  [Constraint] ->
  m (Subst UniVar, [RecordConstraint UniVar UniVar])
solveConstraints cs = do
  ((_, residual), s) <- runStateT solve Subst.empty
  pure (s, residual)
  where
    solve = untilM (null . fst) step (foldMap split cs)
    split (EqualityConstraint c) = ([c], mempty)
    split (RecordConstraint c) = (mempty, [c])
    step (eqs1, recs) = do
      logInfoDoc $ "step:" <+> group (line <> vsep [pretty eqs1, pretty recs])
      s <- solveEqualityConstraints eqs1
      logInfoDoc $ "eq:" <+> group (line <> pretty eqs1 <> line <> "=>" <+> pretty s)
      modify $ Subst.compose s
      (recs', eqs2) <- solveRecordConstraints $ fmap (Subst.substitute s . f) recs
      logInfoDoc $
        "rec:"
          <+> group
            ( line
                <> pretty (fmap (Subst.substitute s . f) recs)
                <> line
                <> "=>"
                <+> group (vsep [pretty recs', pretty eqs2])
            )
      logInfoDoc $ "step =>" <+> pretty eqs2
      pure (eqs2, recs')
    f (fs :<: u) = fs :<: UniType u

solveEqualityConstraints ::
  ( MonadError TypeError m,
    MonadLogger m
  ) =>
  [EqualityConstraint] ->
  m (Subst UniVar)
solveEqualityConstraints = foldrM f Subst.empty
  where
    f c s1 = do
      s2 <- solveEqualityConstraint (Subst.substitute s1 c)
      pure $ Subst.compose s2 s1

solveEqualityConstraint ::
  ( MonadError TypeError m,
    MonadLogger m
  ) =>
  EqualityConstraint ->
  m (Subst UniVar)
solveEqualityConstraint (UniType u :~: t) = unifyVar u t
solveEqualityConstraint (t :~: UniType u) = unifyVar u t
solveEqualityConstraint (ApplyType k1 ts1 :~: ApplyType k2 ts2)
  | k1 == k2 && length ts1 == length ts2 =
      solveEqualityConstraints . Vector.toList $ Vector.zipWith (:~:) ts1 ts2
solveEqualityConstraint (RecordType fs1 :~: RecordType fs2)
  | HashMap.keysSet fs1 == HashMap.keysSet fs2 =
      solveEqualityConstraints $ HashMap.elems intersection
  where
    intersection = HashMap.intersectionWith (:~:) fs1 fs2
solveEqualityConstraint (t1 :~: t2)
  | t1 == t2 = pure Subst.empty
  | otherwise = throwError $ MismatchedType t1 t2

unifyVar ::
  ( MonadError TypeError m
  ) =>
  UniVar ->
  Monotype UniVar ->
  m (Subst UniVar)
unifyVar u t
  | HashSet.member u (fuv t) = throwError $ OccurCheck (UniType u) t
  | otherwise = pure $ Subst.singleton u t

solveRecordConstraints ::
  ( MonadError TypeError m,
    MonadLogger m
  ) =>
  [RecordConstraint (Monotype UniVar) UniVar] ->
  m ([RecordConstraint UniVar UniVar], [EqualityConstraint])
solveRecordConstraints cs = runWriterT $ do
  m <- foldrM f mempty cs
  mapMaybeM solveRecordConstraint (g <$> Map.toList m)
  where
    f (fs1 :<: t) acc = case Map.lookup t acc of
      Nothing -> pure $ Map.insert t fs1 acc
      Just fs2 -> do
        tell . HashMap.elems $ HashMap.intersectionWith (:~:) fs1 fs2
        pure $ Map.insert t (HashMap.union fs1 fs2) acc
    g (t, fs) = fs :<: t

solveRecordConstraint ::
  ( MonadError TypeError m,
    MonadLogger m,
    MonadWriter [EqualityConstraint] m
  ) =>
  RecordConstraint (Monotype UniVar) UniVar ->
  m (Maybe (RecordConstraint UniVar UniVar))
solveRecordConstraint (fs1 :<: RecordType fs2)
  | HashMap.null (fs1 `HashMap.difference` intersection) = do
      tell $ HashMap.elems intersection
      pure Nothing
  | otherwise = throwError $ MismatchedRecord fs1 (RecordType fs2)
  where
    intersection = HashMap.intersectionWith (:~:) fs1 fs2
solveRecordConstraint (fs1 :<: UniType u) = pure $ Just (fs1 :<: u)
solveRecordConstraint (fs1 :<: t) = throwError $ MismatchedRecord fs1 t

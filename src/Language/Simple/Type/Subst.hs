{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Subst
  ( Subst (..),
    limit,
    compose,
    domain,
    null,
    lookup,
    empty,
    member,
    singleton,
    fromBinders,
    Unifier,
    Substitutable (..),
  )
where

import Data.Foldable (foldlM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
  ( empty,
    insert,
    intersection,
    keysSet,
    lookup,
    member,
    null,
    singleton,
    toList,
    union,
  )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet (toMap)
import Data.Hashable (Hashable)
import Fresh (Fresh (..))
import Language.Simple.Syntax (Monotype (..), TypeScheme (..), TypeVar)
import Language.Simple.Type.Constraint (UniVar)
import Prettyprinter (Pretty (..), list, (<+>))
import Util (fromJustOr)
import Prelude hiding (lookup, null)

newtype Subst a = Subst (HashMap a (Monotype UniVar))

instance Pretty a => Pretty (Subst a) where
  pretty (Subst m) = list . map f $ HashMap.toList m
    where
      f (k, v) = pretty k <+> "↦" <+> pretty v

type Unifier = Subst UniVar

empty :: Subst a
empty = Subst HashMap.empty

null :: Subst a -> Bool
null (Subst m) = HashMap.null m

member :: (Hashable a, Eq a) => a -> Subst a -> Bool
member k (Subst m) = HashMap.member k m

lookup :: (Hashable a, Eq a) => a -> Subst a -> Maybe (Monotype UniVar)
lookup k (Subst m) = HashMap.lookup k m

domain :: Subst a -> HashSet a
domain (Subst m) = HashMap.keysSet m

limit :: (Hashable a, Eq a) => HashSet a -> Subst a -> Subst a
limit s (Subst m) = Subst . HashMap.intersection m $ HashSet.toMap s

singleton :: Hashable a => a -> Monotype UniVar -> Subst a
singleton k v = Subst $ HashMap.singleton k v

compose ::
  ( Eq a,
    Hashable a,
    Substitutable a (Monotype UniVar)
  ) =>
  Subst a ->
  Subst a ->
  Subst a
compose (Subst m1) (Subst m2) = Subst $ HashMap.union (fmap (substitute (Subst m1)) m2) m1

class Substitutable a b where
  substitute :: Subst a -> b -> b

instance Substitutable UniVar (Monotype UniVar) where
  substitute _ (VarType v) = VarType v
  substitute s (ApplyType k ts) = ApplyType k $ fmap (substitute s) ts
  substitute (Subst s) (UniType u) = HashMap.lookup u s `fromJustOr` UniType u

instance Substitutable UniVar (TypeScheme UniVar) where
  substitute s ForallTypeScheme {vars, monotype} = ForallTypeScheme {vars, monotype = substitute s monotype}

instance Substitutable TypeVar (Monotype UniVar) where
  substitute (Subst s) (VarType v) = HashMap.lookup v s `fromJustOr` VarType v
  substitute s (ApplyType k ts) = ApplyType k $ fmap (substitute s) ts
  substitute _ (UniType v) = UniType v

fromBinders :: (Eq a, Hashable a, Fresh m, Foldable f) => f a -> m (Subst a)
fromBinders = foldlM go empty
  where
    go (Subst subst) v = do
      a <- fresh
      pure . Subst $ HashMap.insert v (UniType a) subst

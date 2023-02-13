{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Constraint
  ( UniVar,
    Constraint (..),
    fuv,
  )
where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet (singleton)
import Data.Hashable (Hashable)
import Fresh (GenFresh (..))
import GHC.Generics (Generic)
import Language.Simple.Syntax (Monotype (..))
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (..), unsafeViaShow, (<+>))

newtype UniVar = UniVar Natural
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance Pretty UniVar where
  pretty (UniVar n) = "'u" <> unsafeViaShow n

instance GenFresh UniVar where
  fromFreshNatural = UniVar

data Constraint = EqualityConstraint (Monotype UniVar) (Monotype UniVar)
  deriving (Generic)

instance Pretty Constraint where
  pretty (EqualityConstraint t1 t2) = pretty t1 <+> "~" <+> pretty t2

fuv :: Monotype UniVar -> HashSet UniVar
fuv (VarType _) = mempty
fuv (ApplyType _ ts) = foldMap fuv ts
fuv (UniType u) = HashSet.singleton u

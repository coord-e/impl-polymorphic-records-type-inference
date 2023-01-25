{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Simple.Type.Constraint
  ( UniVar,
    Constraint (..),
  )
where

import Data.Hashable (Hashable)
import Fresh (GenFresh (..))
import GHC.Generics (Generic)
import Language.Simple.Syntax (Monotype (..))
import Numeric.Natural (Natural)

newtype UniVar = UniVar Natural
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance GenFresh UniVar where
  fromFreshNatural = UniVar

data Constraint = EqualityConstraint (Monotype UniVar) (Monotype UniVar)
  deriving (Generic)

{-# LANGUAGE DeriveGeneric #-}

module Language.Simple.Type.Error
  ( TypeError (..),
    TypeException (..),
  )
where

import Control.Exception (Exception)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Language.Simple.Syntax (DataCtor, Label, Monotype, RecordConstraint, TermVar, TypeVar)
import Language.Simple.Type.UniVar (UniVar)

data TypeError
  = UnboundTermVar TermVar
  | UnboundTypeVar TypeVar
  | UnboundDataCtor DataCtor
  | MismatchedType (Monotype UniVar) (Monotype UniVar)
  | MismatchedRecord (HashMap Label (Monotype UniVar)) (Monotype UniVar)
  | OccurCheck (Monotype UniVar) (Monotype UniVar)
  deriving (Generic, Show)

data TypeException
  = FreeUniVarInConstraintException (RecordConstraint UniVar UniVar)
  | FreeTypeVarInConstraintException (RecordConstraint TypeVar UniVar)
  deriving (Generic, Show)

instance Exception TypeException

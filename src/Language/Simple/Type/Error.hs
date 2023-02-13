{-# LANGUAGE DeriveGeneric #-}

module Language.Simple.Type.Error
  ( TypeError (..),
  )
where

import GHC.Generics (Generic)
import Language.Simple.Syntax (DataCtor, Monotype, TermVar, TypeVar)
import Language.Simple.Type.UniVar (UniVar)

data TypeError
  = UnboundTermVar TermVar
  | UnboundTypeVar TypeVar
  | UnboundDataCtor DataCtor
  | MismatchedType (Monotype UniVar) (Monotype UniVar)
  | OccurCheck (Monotype UniVar) (Monotype UniVar)
  deriving (Generic, Show)

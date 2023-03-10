{-# LANGUAGE DeriveGeneric #-}

module Language.Simple.Type.Error
  ( TypeError (..),
    TypeException (..),
  )
where

import Control.Exception (Exception)
import GHC.Generics (Generic)
import Language.Simple.Syntax (DataCtor, Kind, Monotype, TermVar, TypeVar)
import Language.Simple.Type.UniVar (UniVar)

data TypeError
  = UnboundTermVar TermVar
  | UnboundTypeVar TypeVar
  | UnboundDataCtor DataCtor
  | MismatchedType (Monotype UniVar) (Monotype UniVar)
  | MismatchedKind UniVar (Kind UniVar) (Kind UniVar)
  | OccurCheck (Monotype UniVar) (Monotype UniVar)
  deriving (Generic, Show)

newtype TypeException
  = UniVarWithoutKindException UniVar
  deriving (Generic, Show)

instance Exception TypeException

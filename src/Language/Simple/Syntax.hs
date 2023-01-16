{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Simple.Syntax
  ( -- * Expressions
    Expr (..),
    TermVar (..),
    DataCtor (..),
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Expression.
data Expr
  = -- | Data constructor. \( K \)
    CtorExpr DataCtor
  | -- | Term variable. \( x \)
    VarExpr TermVar
  | -- | Function abstraction. \( \lambda x . e \)
    LambdaExpr TermVar Expr
  | -- | Function application. \( e_1 e_2 \)
    ApplyExpr Expr Expr
  | -- | Let binding. \( \texttt{let} x = e_1 \texttt{in} e_2 \)
    LetExpr TermVar Expr Expr
  deriving stock (Show, Generic)

-- | Term-level variable.
newtype TermVar = TermVar Text
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

-- | Data constructor.
data DataCtor
  = NamedDataCtor Text
  | IntegerDataCtor Integer
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (Hashable)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Syntax
  ( -- * Expressions
    Expr (..),
    TermVar (..),
    DataCtor (..),

    -- * Types
    TypeScheme (..),
    Monotype (..),
    RigidMonotype,
    functionType,
    TypeVar (..),
    TypeCtor (..),
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Data.Void (Void)
import Fresh (GenFresh (..))
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

-- | Type scheme that contains unification variable as @a@.
data TypeScheme a = ForallTypeScheme
  { -- | Type variables to be quantified.
    vars :: Vector TypeVar,
    -- | The type.
    monotype :: Monotype a
  }
  deriving (Generic)

-- | Monotype that contains unification variable as @a@.
data Monotype a
  = -- | Rigid type variable. \( a \)
    VarType TypeVar
  | -- | Type constructor application. \( T \bar{\tau} \)
    ApplyType TypeCtor (Vector (Monotype a))
  | -- | Unification type variable. \( \alpha \)
    UniType a
  deriving (Generic, Show)

type RigidMonotype = Monotype Void

functionType :: Monotype a -> Monotype a -> Monotype a
functionType a b = ApplyType FunctionTypeCtor $ Vector.fromList [a, b]

-- | Type constructor.
data TypeCtor
  = NamedTypeCtor Text
  | FunctionTypeCtor
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (Hashable)

-- | Type-level variable.
newtype TypeVar = TypeVar Text
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance GenFresh TypeVar where
  fromFreshNatural n = TypeVar $ "a" <> pack (show n)

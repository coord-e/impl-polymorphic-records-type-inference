{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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

    -- * Others
    Label (..),
    Kind (..),
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (toList)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, toList)
import Data.Void (Void)
import Fresh (GenFresh (..))
import GHC.Generics (Generic)
import Prettyprinter
  ( Doc,
    Pretty (..),
    group,
    hsep,
    line,
    nest,
    parens,
    punctuate,
    space,
    vsep,
    (<+>),
  )
import Prettyprinter.Internal (unsafeTextWithoutNewlines)

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
  | -- | Record constructor. \( \{ \bar{l \texttt{=} e} \} \)
    RecordExpr (HashMap Label Expr)
  | -- | Member update. \( \{ \bar{l \texttt{=} e}, ..e \} \)
    UpdateExpr (HashMap Label Expr) Expr
  | -- | Member access. \( e \texttt{.} l \)
    MemberExpr Expr Label
  deriving stock (Show, Generic)

instance Pretty Expr where
  pretty (CtorExpr k) = pretty k
  pretty (VarExpr x) = pretty x
  pretty (LambdaExpr x e) = group ("\\" <> pretty x <> "." <> nest 2 (line <> pretty e))
  pretty (ApplyExpr e1 e2) = nested e1 <+> prettyAtomExpr e2
    where
      nested t@(ApplyExpr {}) = pretty t
      nested t@(CtorExpr {}) = pretty t
      nested t@(VarExpr {}) = pretty t
      nested t = parens (pretty t)
  pretty (LetExpr x e1 e2) =
    group
      ( "let"
          <+> pretty x
          <+> "="
            <> nest 2 (line <> pretty e1)
            <> line
            <> "in"
          <+> pretty e2
      )
  pretty (RecordExpr fs) = group $ "{" <> nest 2 (line <> vsep fs') <> line <> "}"
    where
      fs' = punctuate ", " (f <$> HashMap.toList fs)
      f (l, e') = pretty l <+> "=" <+> pretty e'
  pretty (UpdateExpr fs e) = group $ "{" <> nest 2 (line <> vsep fs') <> line <> "}"
    where
      fs' = punctuate ", " (fmap f (HashMap.toList fs) ++ [".." <> pretty e])
      f (l, e') = pretty l <+> "=" <+> pretty e'
  pretty (MemberExpr e l) = prettyAtomExpr e <> "." <> pretty l

prettyAtomExpr :: Expr -> Doc ann
prettyAtomExpr t@(LambdaExpr {}) = parens (pretty t)
prettyAtomExpr t@(ApplyExpr {}) = parens (pretty t)
prettyAtomExpr t@(LetExpr {}) = parens (pretty t)
prettyAtomExpr t = pretty t

-- | Record label.
newtype Label = Label Text
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance Pretty Label where
  pretty (Label x) = unsafeTextWithoutNewlines x

-- | Term-level variable.
newtype TermVar = TermVar Text
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance Pretty TermVar where
  pretty (TermVar x) = unsafeTextWithoutNewlines x

-- | Data constructor.
data DataCtor
  = NamedDataCtor Text
  | IntegerDataCtor Integer
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (Hashable)

instance Pretty DataCtor where
  pretty (NamedDataCtor k) = unsafeTextWithoutNewlines k
  pretty (IntegerDataCtor i) = pretty i

-- | Type scheme that contains unification variable as @a@.
data TypeScheme a = ForallTypeScheme
  { -- | Type variables to be quantified.
    vars :: Vector (TypeVar, Kind a),
    -- | The type.
    monotype :: Monotype a
  }
  deriving (Generic)

instance Pretty a => Pretty (TypeScheme a) where
  pretty ForallTypeScheme {vars, monotype} = quant <> pretty monotype
    where
      quant
        | null vars = mempty
        | otherwise = "∀" <+> hsep (map var (Vector.toList vars)) <> "." <> space
      var (v, k) = pretty v <> "::" <> pretty k

-- | Monotype that contains unification variable as @a@.
data Monotype a
  = -- | Rigid type variable. \( a \)
    VarType TypeVar
  | -- | Type constructor application. \( T \bar{\tau} \)
    ApplyType TypeCtor (Vector (Monotype a))
  | -- | Record type. \( \{ \bar{l \texttt{:} \tau} \} \)
    RecordType (HashMap Label (Monotype a))
  | -- | Unification type variable. \( \alpha \)
    UniType a
  deriving (Generic, Show)

type RigidMonotype = Monotype Void

functionType :: Monotype a -> Monotype a -> Monotype a
functionType a b = ApplyType FunctionTypeCtor $ Vector.fromList [a, b]

instance Pretty a => Pretty (Monotype a) where
  pretty (VarType v) = pretty v
  pretty (UniType a) = pretty a
  pretty (ApplyType FunctionTypeCtor (Vector.toList -> [a, b]))
    | isNested a = parens (pretty a) <+> "->" <+> pretty b
    | otherwise = pretty a <+> "->" <+> pretty b
    where
      isNested (ApplyType FunctionTypeCtor _) = True
      isNested _ = False
  pretty (ApplyType k ts) = hsep (pretty k : map prettyAtomMonotype (Vector.toList ts))
  pretty (RecordType fs) = group ("{" <> nest 2 (line <> vsep fs') <> line <> "}")
    where
      fs' = punctuate ", " (f <$> HashMap.toList fs)
      f (l, t) = pretty l <+> ":" <+> pretty t

prettyAtomMonotype :: Pretty a => Monotype a -> Doc ann
prettyAtomMonotype t@(ApplyType _ ts') | not (null ts') = parens (pretty t)
prettyAtomMonotype t = pretty t

-- | Type constructor.
data TypeCtor
  = NamedTypeCtor Text
  | FunctionTypeCtor
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (Hashable)

instance Pretty TypeCtor where
  pretty (NamedTypeCtor k) = unsafeTextWithoutNewlines k
  pretty FunctionTypeCtor = "(->)"

-- | Type-level variable.
newtype TypeVar = TypeVar Text
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Show)
  deriving anyclass (Hashable)

instance GenFresh TypeVar where
  fromFreshNatural n = TypeVar $ "a" <> pack (show n)

instance Pretty TypeVar where
  pretty (TypeVar v) = unsafeTextWithoutNewlines v

-- | Kind of types that contains unification variable as @a@.
data Kind a
  = -- | Types. \( U \)
    TypeKind
  | -- | Records. \( \{\!\{ \bar{l \texttt{:} \tau} \}\!\} \)
    RecordKind (HashMap Label (Monotype a))
  deriving (Generic, Show)

instance Pretty a => Pretty (Kind a) where
  pretty TypeKind = "U"
  pretty (RecordKind fs) = group ("{{" <> nest 2 (line <> vsep fs') <> line <> "}}")
    where
      fs' = punctuate ", " (f <$> HashMap.toList fs)
      f (l, t) = pretty l <+> ":" <+> pretty t

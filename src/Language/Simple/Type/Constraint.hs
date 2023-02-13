{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Type.Constraint
  ( Constraint (..),
  )
where

import GHC.Generics (Generic)
import Language.Simple.Syntax (Monotype)
import Language.Simple.Type.UniVar (UniVar)
import Prettyprinter (Pretty (..), (<+>))

data Constraint = EqualityConstraint (Monotype UniVar) (Monotype UniVar)
  deriving (Generic)

instance Pretty Constraint where
  pretty (EqualityConstraint t1 t2) = pretty t1 <+> "~" <+> pretty t2

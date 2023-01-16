{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Simple.Parser
  ( ParseError (..),
    parseExpr,

    -- * Parsers
    exprParser,
    termVarParser,
    dataCtorParser,
    namedDataCtorParser,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (MonadError (..))
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable (foldl')
import Data.Text (Text, pack)
import Language.Simple.Syntax
  ( DataCtor (..),
    Expr (..),
    TermVar (..),
  )
import Text.Parser.Char (CharParsing (..), alphaNum, lower, text, upper)
import Text.Parser.Combinators
  ( Parsing (..),
    choice,
  )
import Text.Parser.Token
  ( TokenParsing (..),
    dot,
    integer,
    parens,
    textSymbol,
    token,
    whiteSpace,
  )
import Text.Parser.Token.Style (buildSomeSpaceParser, scalaCommentStyle)

newtype ParseError = ParseError {errorMessage :: String}
  deriving stock (Show)

parseExpr ::
  MonadError ParseError m =>
  Text ->
  m Expr
parseExpr input =
  case parseOnly parser input of
    Left errorMessage -> throwError $ ParseError {errorMessage}
    Right x -> pure x
  where
    parser = runComment (whiteSpace *> exprParser <* eof)

newtype Comment m a = Comment {runComment :: m a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing)

instance TokenParsing m => TokenParsing (Comment m) where
  someSpace = buildSomeSpaceParser (Comment someSpace) scalaCommentStyle
  nesting (Comment p) = Comment $ nesting p
  semi = Comment semi
  highlight h (Comment p) = Comment $ highlight h p

exprParser :: TokenParsing m => m Expr
exprParser = foldl' ApplyExpr <$> atom <*> many atom <?> "expression"
  where
    atom =
      parens exprParser
        <|> lambda
        <|> let_
        <|> CtorExpr <$> dataCtorParser
        <|> VarExpr <$> termVarParser
    lambda = textSymbol "\\" *> (LambdaExpr <$> (termVarParser <* dot) <*> exprParser)
    let_ =
      LetExpr
        <$> (keyword "let" *> termVarParser)
        <*> (textSymbol "=" *> exprParser)
        <*> (keyword "in" *> exprParser)

termVarParser :: TokenParsing m => m TermVar
termVarParser = TermVar <$> lowerName <?> "variable"

namedDataCtorParser :: TokenParsing m => m DataCtor
namedDataCtorParser = NamedDataCtor <$> upperName

dataCtorParser :: TokenParsing m => m DataCtor
dataCtorParser = IntegerDataCtor <$> integer <|> namedDataCtorParser <?> "data constructor"

lowerName :: TokenParsing m => m Text
lowerName = fmap pack . token . try $ notFollowedBy anyKeyword *> name
  where
    name = (:) <$> lower <*> many alphaNum

upperName :: TokenParsing m => m Text
upperName = pack <$> token name
  where
    name = (:) <$> upper <*> many alphaNum

anyKeyword :: TokenParsing m => m Text
anyKeyword = choice (map keyword keywords)
  where
    keywords = ["let", "in"]

keyword :: TokenParsing m => Text -> m Text
keyword x = token . try $ text x <* notFollowedBy alphaNum

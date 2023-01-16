{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( App,
    runApp,
    parseExpr,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LoggingT, MonadLogger, logErrorN, runStdoutLoggingT)
import Data.Text (Text, pack)
import qualified Language.Simple.Parser as Parser (errorMessage, parseExpr)
import Language.Simple.Syntax (Expr)
import System.Exit (exitFailure)

newtype App a = App (ExceptT String (LoggingT IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)

parseExpr :: Text -> App Expr
parseExpr = App . withExceptT Parser.errorMessage . Parser.parseExpr

runApp :: App a -> IO a
runApp (App a) = runStdoutLoggingT $ do
  e <- runExceptT a
  case e of
    Left err -> logErrorN (pack err) >> liftIO exitFailure
    Right x -> pure x

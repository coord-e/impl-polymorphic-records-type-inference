{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import Language.Simple.Parser (parseExpr)
import qualified Language.Simple.Parser as Parser (ParseError (..))
import Language.Simple.Type (typeExpr)
import System.Directory (listDirectory)
import System.FilePath.Posix (isExtensionOf)
import Test.Tasty
import Test.Tasty.HUnit

dataDir :: FilePath
dataDir = "test/data/"

assertion :: Bool -> Text -> Assertion
assertion shouldPass content = runNoLoggingT $ do
  expr <-
    runExceptT (parseExpr content) >>= \case
      Left err -> liftIO . assertFailure $ Parser.errorMessage err
      Right x -> pure x
  runExceptT (typeExpr expr) >>= \case
    Left err | shouldPass -> liftIO . assertFailure $ show err
    Right _ | not shouldPass -> liftIO $ assertFailure "unexpectedly typechecked"
    _ -> pure ()

makeTestForFile :: FilePath -> IO TestTree
makeTestForFile path = do
  content <- Text.readFile (dataDir ++ path)
  pure . testCase path $ assertion shouldPass content
  where
    shouldPass = isExtensionOf "pass" path

main :: IO ()
main = do
  files <- listDirectory dataDir
  tests <- mapM makeTestForFile files
  defaultMain $ testGroup "tests" tests

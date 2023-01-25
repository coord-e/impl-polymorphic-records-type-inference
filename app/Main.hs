module Main where

import App
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as Text (readFile)
import System.Environment (getArgs)

typeFile :: FilePath -> App ()
typeFile file = do
  content <- liftIO $ Text.readFile file
  expr <- parseExpr content
  liftIO . putStrLn $ show expr
  typeExpr expr

main :: IO ()
main = do
  [path] <- getArgs
  runApp $ typeFile path

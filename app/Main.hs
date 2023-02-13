module Main where

import App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN)
import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)

showPretty :: Pretty a => a -> Text
showPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

typeFile :: FilePath -> App ()
typeFile file = do
  content <- liftIO $ Text.readFile file
  expr <- parseExpr content
  logInfoN $ showPretty expr
  typeExpr expr

main :: IO ()
main = do
  [path] <- getArgs
  runApp $ typeFile path

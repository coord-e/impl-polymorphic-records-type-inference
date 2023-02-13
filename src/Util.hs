module Util
  ( orThrow,
    orThrowM,
    fromJustOr,
    showPretty,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow (Just x) _ = pure x
orThrow Nothing e = throwError e

orThrowM :: MonadError e m => m (Maybe a) -> e -> m a
orThrowM a e = do
  m <- a
  m `orThrow` e

fromJustOr :: Maybe a -> a -> a
fromJustOr (Just x) _ = x
fromJustOr Nothing x = x

showPretty :: Pretty a => a -> Text
showPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

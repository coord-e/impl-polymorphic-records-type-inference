module Util
  ( orThrow,
    orThrowM,
    fromJustOr,
    logInfoDoc,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
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

logInfoDoc :: MonadLogger m => Doc ann -> m ()
logInfoDoc = logInfoN . renderStrict . layoutPretty defaultLayoutOptions

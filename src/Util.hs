module Util
  ( orThrow,
    orThrowM,
    fromJustOr,
    logInfoDoc,
    untilM,
    mapMaybeM,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Maybe (catMaybes)
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

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f a
  | p a = pure a
  | otherwise = f a >>= untilM p f

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f

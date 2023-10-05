module MonadicFlow where

import Data.Function (fix)
import Essentials

whileJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJust condition action = fix \r ->
  condition >>= maybe (pure ()) \x -> action x *> r

unless :: Monad m => m Bool -> m () -> m ()
unless condition action =
  condition >>= \case
    True -> pure ()
    False -> action

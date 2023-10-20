module Exe.Flags (flagsReader) where

import CabalFile
import Data.Function (flip)
import Data.List (elem)
import Data.Text (Text)
import Data.Text qualified as T
import Essentials
import Map (Map)
import Map qualified
import Options.Applicative
import Prelude (Char)

-- | Optparse reader for Cabal flags
--
-- Flags can be separated by commas, spaces, or both.
--
-- "+flagname" set a flag to True; "-flagname" sets it to false.
flagsReader :: ReadM (Map FlagName Bool)
flagsReader = maybeReader $ flagMapFromText . Text.pack

flagMapFromText :: Text -> Maybe (Map FlagName Bool)
flagMapFromText =
  Text.split (flip (elem @[]) " ,")
    >>> traverse flagFromText
    >>> fmap Map.fromList

flagFromText :: Text -> Maybe (FlagName, Bool)
flagFromText a = do
  (x, y) <- Text.uncons a
  s <- boolFromFlagChar x
  Just (fromText y, s)

boolFromFlagChar :: Char -> Maybe Bool
boolFromFlagChar = \case
  '-' -> Just False
  '+' -> Just True
  _ -> Nothing

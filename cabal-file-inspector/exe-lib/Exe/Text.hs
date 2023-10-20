module Exe.Text where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Essentials
import Prelude (Char, const, either)

-- | Splits a text value into two parts on the first occurrence of the search character
--
-- The results do not include the search character.
--
-- Fails if no character in the input text satisfies the predicate.
splitIn2 :: (Char -> Bool) -> Text -> Maybe (Text, Text)
splitIn2 f =
  Text.break f >>> \(x, y) ->
    if Text.null y then Nothing else Just (x, Text.drop 1 y)

-- | Run a 'Text.Reader', requiring that it read the entire input, discarding error information
textReadOnly :: Text.Reader a -> Text -> Maybe a
textReadOnly r t =
  r t
    & either
      (const Nothing)
      (\(i, remainder) -> if Text.null remainder then Just i else Nothing)

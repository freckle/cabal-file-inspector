module CabalFile.Text where

import Cabal.ShortText qualified as Cabal (ShortText)
import Data.Text (Text)
import Optics
import Prelude (String)

class IsText a where
  string :: Iso' a String
  shortText :: Iso' a Cabal.ShortText
  text :: Iso' a Text

isoViaShortText :: (IsText a, IsText b) => Iso' a b
isoViaShortText = shortText % re shortText

toText :: IsText a => a -> Text
toText = view text

fromText :: IsText a => Text -> a
fromText = review text

module CabalFile.FlagName where

import Cabal.Flag qualified as Cabal (FlagName)
import Cabal.Optics qualified
import Cabal.ShortText qualified
import CabalFile.CabalOptic
import CabalFile.Text
import Data.String (IsString)
import Data.Text qualified as T
import Essentials
import Optics (An_Iso, coerced, iso, (%))

newtype FlagName = FlagName {coerce :: Cabal.FlagName}
  deriving newtype (Eq, Ord, Show, IsString)

instance CabalOptic An_Iso FlagName FlagName Cabal.FlagName Cabal.FlagName where
  cabal = coerced

instance IsText FlagName where
  string = cabal % Cabal.Optics.flagName_string
  shortText = string % iso Cabal.ShortText.toShortText Cabal.ShortText.fromShortText
  text = string % iso Text.pack Text.unpack

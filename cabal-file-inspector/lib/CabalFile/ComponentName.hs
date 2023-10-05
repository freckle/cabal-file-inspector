module CabalFile.ComponentName where

import Cabal.Optics qualified
import Cabal.UnqualComponentName qualified as Cabal (UnqualComponentName)
import CabalFile.CabalOptic
import CabalFile.Text
import Data.String (IsString)
import Data.Text qualified as Text
import Essentials
import Optics (An_Iso, coerced, iso, (%))

-- | The name of a library, test suite, executable, etc. with a .cabal file
--
-- This is just a string; it contains no indication as to what type of component
-- we're talking about.
newtype ComponentName = ComponentName
  { coerce :: Cabal.UnqualComponentName
  }
  deriving newtype (Eq, Ord, Show, IsString)

instance
  CabalOptic
    An_Iso
    ComponentName
    ComponentName
    Cabal.UnqualComponentName
    Cabal.UnqualComponentName
  where
  cabal = coerced

instance IsText ComponentName where
  string = cabal % Cabal.Optics.componentName_string
  shortText = cabal % Cabal.Optics.componentName_shortText
  text = string % iso Text.pack Text.unpack

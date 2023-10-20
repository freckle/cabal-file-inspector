module CabalFile.PackageName where

import Cabal.Optics qualified
import Cabal.PackageName qualified as Cabal (PackageName)
import CabalFile.CabalOptic
import CabalFile.Text
import Data.String (IsString)
import Data.Text qualified as T
import Essentials
import Optics

-- | The value of the "name" field in a .cabal file
newtype PackageName = PackageName
  { coerce :: Cabal.PackageName
  }
  deriving newtype (Eq, Ord, Show, IsString)

class HasPackageName a where
  packageName :: a -> PackageName

instance CabalOptic An_Iso PackageName PackageName Cabal.PackageName Cabal.PackageName where
  cabal = coerced

instance IsText PackageName where
  string = cabal % Cabal.Optics.packageName_string
  shortText = cabal % Cabal.Optics.packageName_shortText
  text = string % iso Text.pack Text.unpack

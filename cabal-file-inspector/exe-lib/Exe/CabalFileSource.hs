module Exe.CabalFileSource
  ( CabalFileSource (..)
  , filePathReader
  , readCabalFile
  )
where

import Cabal.Parsec.Error
import CabalFile
import Control.Monad.Trans.Class (lift)
import Control.Monad.Validate
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Essentials
import Exe.Error
import Exe.Input
import Options.Applicative
import Prelude (String, either)

-- | User's indication of where to find the .cabal file they're inspecting
--
-- The @fp@ parameter is the operating system file path type, e.g. 'OsPath'.
data CabalFileSource fp
  = -- | The contents of a .cabal file will be piped in via the standard input stream
    StandardInputCabalFile
  | -- | The .cabal file is designated by a filesystem path
    CabalFilePath (FilePath fp)
  deriving stock (Functor)

-- | Prints the source like a pseudo-filepath, for use in parsing error messages
showCabalFileSource :: CabalFileSource fp -> String
showCabalFileSource = \case
  StandardInputCabalFile -> "<stdin>"
  CabalFilePath x -> T.unpack x.text

-- | Optparse reader for a file path
--
-- This is parameterized on the 'Input' because different systems can have different
-- rules for what constitutes a valid file path.
filePathReader :: Input fp m -> ReadM (FilePath fp)
filePathReader i = maybeReader \s -> let t = T.pack s in FilePath t <$> i.textFilePath t

cabalParseError :: CabalFileSource fp -> PError -> Error
cabalParseError cabalFileSource = Error . T.pack . showPError (showCabalFileSource cabalFileSource)

-- | Fetch the bytes of .cabal file, and parse
--
-- Can produce validation failure either from the fetching or from the parsing.
readCabalFile
  :: Monad m
  => Input fp m
  -> CabalFileSource fp
  -> ValidateT (NonEmpty Error) m CabalFile
readCabalFile i s = do
  cabalFileBytes <- case s of
    StandardInputCabalFile -> lift $ i.readStandardInput
    CabalFilePath filePath -> lift $ i.readFile filePath.os

  (CabalFile.parseCabalFile cabalFileBytes).result
    & either (\failure -> refute $ cabalParseError s <$> failure.errors) pure

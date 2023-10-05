module CabalFile.CabalFile where

import Cabal.Compiler qualified as Cabal (CompilerInfo)
import Cabal.ComponentRequestedSpec qualified
import Cabal.ComponentRequestedSpec qualified as Cabal
  ( ComponentRequestedSpec (ComponentRequestedSpec)
  )
import Cabal.Dependency qualified as Cabal (Dependency)
import Cabal.Flag qualified
import Cabal.Flag qualified as Cabal (FlagAssignment)
import Cabal.GenericPackageDescription qualified as Cabal
  ( GenericPackageDescription
  )
import Cabal.Optics qualified
import Cabal.PackageDescription qualified as Cabal (PackageDescription)
import Cabal.PackageDescription.Configuration qualified as Cabal.Configuration
  ( finalizePD
  )
import Cabal.PackageDescription.Parsec qualified as Cabal.Parsec
  ( parseGenericPackageDescription
  , runParseResult
  )
import Cabal.PackageVersionConstraint qualified as Cabal
  ( PackageVersionConstraint
  )
import Cabal.Parsec.Error qualified as Cabal (PError)
import Cabal.Parsec.Warning qualified as Cabal (PWarning)
import Cabal.System qualified as Cabal (Platform)
import Cabal.Version qualified as Cabal (Version)
import CabalFile.CabalOptic
import CabalFile.FlagName
import CabalFile.Package
import CabalFile.PackageName
import Control.Exception (Exception)
import Control.Monad.Validate
import Data.ByteString qualified as ByteString.Strict
import Data.Either qualified as Either
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.String (IsString)
import Data.String qualified as IsString (IsString (..))
import Data.Text qualified as Text.Strict
import Data.Text.Encoding qualified as Text.Strict
import Essentials
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Map (Map)
import Map qualified
import Optics
import Prelude (Either, const, either, error, show)

-- | Abstract representation of a .cabal file
--
-- Although we often think of a .cabal as corresponding to a Haskell package, here we take
-- more precise view that it is a /function/ which, given 'CabalArguments', returns a package.
-- (See 'applyCabalArguments'.)
newtype CabalFile = CabalFile
  { coerce :: Cabal.GenericPackageDescription
  }

-- | The arguments required to resolve a 'CabalFile' into a 'Package'
data CabalArguments = CabalArguments
  { flags :: Map FlagName Bool
  , platform :: Cabal.Platform
  , compiler :: Cabal.CompilerInfo
  }
  deriving stock (Show)

type FlagAssignment = Cabal.FlagAssignment

-- | What information we get when trying to read a malformed .cabal file
data CabalReadFailure = CabalReadFailure
  { version :: Maybe Cabal.Version
  , errors :: NonEmpty Cabal.PError
  }
  deriving stock (Show)

-- | Context representing the various things that can go wrong when parsing a cabal file
data CabalRead a = CabalRead
  { warnings :: [Cabal.PWarning]
  , result :: Either CabalReadFailure a
  }
  deriving stock (Functor)

-- | Anything that might be concerning when reading a .cabal file; either a warning or an error
data CabalReadIssue
  = CabalReadWarning Cabal.PWarning
  | CabalReadError Cabal.PError
  deriving stock (Show)

-- | A non-empty list of warnings or errors arising from reading a .cabal file
newtype CabalReadIssues = CabalReadIssues
  { list :: NonEmpty CabalReadIssue
  }
  deriving stock (Show)
  deriving newtype (Semigroup)

data CabalReadException e
  = HasCallStack => CabalReadException e
  deriving anyclass (Exception)

instance Show e => Show (CabalReadException e) where
  show (CabalReadException e) =
    "CabalReadException\n" <> show e <> "\n" <> prettyCallStack callStack

-- | Parses like the contents of a .cabal file. Ignores warnings. Bottom if parsing fails.
instance IsString CabalFile where
  fromString =
    Text.Strict.pack
      >>> Text.Strict.encodeUtf8
      >>> parseCabalFile
      >>> (.result)
      >>> either (error . show) id

-- | Interpret a byte string as a .cabal file, hopefully generating a 'CabalFile',
--   possibly generating warnings and errors
parseCabalFile :: ByteString.Strict.ByteString -> CabalRead CabalFile
parseCabalFile =
  Cabal.Parsec.parseGenericPackageDescription
    >>> Cabal.Parsec.runParseResult
    >>> \(w, r) ->
      CabalRead
        { warnings = w
        , result = case r of
            Either.Left (v, es) -> Either.Left $ CabalReadFailure v es
            Either.Right x -> Either.Right $ x ^. re cabal
        }

-- | Consolidate /warnings/ and /errors/ into "issues"
--
-- This can be useful in situations where you aren't willing to tolerate any nonsense
-- and thus don't much care the difference between a warning and an error.
pedanticCabalRead :: HasCallStack => CabalRead a -> Validate CabalReadIssues a
pedanticCabalRead x =
  traverse_ raiseWarnings (nonEmpty x.warnings)
    *> either (raiseErrors . (.errors)) pure x.result

raiseErrors
  :: HasCallStack => NonEmpty Cabal.PError -> ValidateT CabalReadIssues Identity a
raiseErrors = refute . CabalReadIssues . fmap CabalReadError

raiseWarnings
  :: HasCallStack
  => NonEmpty Cabal.PWarning
  -> ValidateT CabalReadIssues Identity ()
raiseWarnings = dispute . CabalReadIssues . fmap CabalReadWarning

applyCabalArguments :: CabalArguments -> CabalFile -> Package
applyCabalArguments arguments cabalFile =
  applyCabalArguments' arguments (cabalFile ^. cabal) & \case
    Either.Left _ -> error "finalizePD failed - This shouldn't happen?"
    Either.Right (x, _) -> x ^. re cabal

applyCabalArguments'
  :: CabalArguments
  -> Cabal.GenericPackageDescription
  -> Either [Cabal.Dependency] (Cabal.PackageDescription, Cabal.FlagAssignment)
applyCabalArguments' arguments =
  Cabal.Configuration.finalizePD
    ( Cabal.Flag.mkFlagAssignment $
        over _1 (view cabal)
          <$> Map.toList arguments.flags
    )
    allComponentsEnabled
    (const True)
    arguments.platform
    arguments.compiler
    ([] :: [Cabal.PackageVersionConstraint])

allComponentsEnabled :: Cabal.ComponentRequestedSpec
allComponentsEnabled =
  Cabal.ComponentRequestedSpec
    { testsRequested = True
    , benchmarksRequested = True
    }

instance
  CabalOptic
    An_Iso
    CabalFile
    CabalFile
    Cabal.GenericPackageDescription
    Cabal.GenericPackageDescription
  where
  cabal = coerced

instance HasPackageName CabalFile where
  packageName = view $ cabal % Cabal.Optics.genericPackageDescription_name % re cabal

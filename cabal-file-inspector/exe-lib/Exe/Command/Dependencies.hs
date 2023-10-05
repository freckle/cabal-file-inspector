module Exe.Command.Dependencies
  ( command
  , Options (..)
  , optionsParser
  , getDependencies
  , getPackage
  )
where

import Cabal.Compiler
  ( AbiTag (..)
  , CompilerId
  , buildCompilerId
  , unknownCompilerInfo
  )
import Cabal.System (Platform, buildPlatform, platformFromTriple)
import CabalFile
  ( CabalArguments (CabalArguments)
  , FlagName
  , Package
  , PackageName
  , QualifiedComponentName
  )
import CabalFile qualified
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Validate
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as Text
import Essentials
import Exe.CabalFileSource
import Exe.Compiler
import Exe.ComponentSpecification
import Exe.Error
import Exe.Flags
import Exe.Input
import Map (Map)
import Options.Applicative
  ( Alternative (many)
  , Parser
  , long
  , maybeReader
  , option
  , optional
  , switch
  )
import Set (Set)
import Set qualified
import System.OsPath (OsPath)
import Prelude (IO, either)

-- | The command itself
command :: Parser (IO ())
command = optionsParser regularInput <&> run regularInput

run :: Input OsPath IO -> Options OsPath -> IO ()
run i opt = do
  either errorQuit pure =<< runValidateT @(NonEmpty Error) @IO do
    getPackage i opt >>= getDependencies opt >>= (lift . printDependencies)

-- | Straightforward representation of successful (but unvalidated) parse
--   of command-line arguments for the command
data Options fp = Options
  { flags :: Map FlagName Bool
  , platform :: Maybe Platform
  , compiler :: Maybe CompilerId
  , allComponents :: Bool
  , mainLibrary :: Bool
  , components :: Set QualifiedComponentName
  , standardInputCabalFile :: Bool
  , cabalFilePath :: Maybe (FilePath fp)
  }

-- | Optparse parser for the command
--
-- This is parameterized on 'Input' because the parameters include file paths,
-- and how file paths are parsed from text depends on the file system.
optionsParser :: Input fp m -> Parser (Options fp)
optionsParser i = do
  flags <- fmap fold $ many $ option flagsReader $ long "flags"
  platform <- optional $ option (maybeReader platformFromTriple) $ long "platform"
  compiler <- optional $ option compilerReader $ long "compiler"
  allComponents <- switch $ long "all-components"
  mainLibrary <- switch $ long "main-library"
  components <- fmap fold $ many $ option componentsReader $ long "components"
  standardInputCabalFile <- switch $ long "standard-input-cabal-file"
  cabalFilePath <- optional $ option (filePathReader i) $ long "cabal-file-path"
  pure Options {..}

cabalArguments :: Options fp -> CabalArguments
cabalArguments opt =
  CabalArguments
    { flags = opt.flags
    , platform = fromMaybe buildPlatform opt.platform
    , compiler = unknownCompilerInfo (fromMaybe buildCompilerId opt.compiler) NoAbiTag
    }

componentSetSpecification :: Options fp -> ComponentSetSpecification
componentSetSpecification opt =
  ComponentSetSpecification
    { allComponents = opt.allComponents
    , components =
        Set.map NamedComponent opt.components
          <> (if opt.mainLibrary then Set.singleton MainLibrary else Set.empty)
    }

validateCabalFileSource
  :: Monad m
  => Input fp m
  -> Options fp
  -> ValidateT (NonEmpty Error) m (CabalFileSource fp)
validateCabalFileSource i opt =
  case (opt.standardInputCabalFile, opt.cabalFilePath) of
    (False, Nothing) ->
      refute $
        pure $
          Error "Need either --cabal-file-path or --standard-input-cabal-file"
    (True, Just {}) ->
      refute $
        pure $
          Error "Cannot do both --cabal-file-path and --standard-input-cabal-file"
    (False, Just cabalFilePath) -> do
      exists <- lift $ i.doesFileExist cabalFilePath.os
      unless exists $
        dispute $
          pure $
            Error $
              "Cabal file path ‘" <> cabalFilePath.text <> "’ is not a file"
      pure $ CabalFilePath cabalFilePath
    (True, Nothing) -> pure StandardInputCabalFile

getDependencies
  :: Monad m
  => Options fp
  -> Package
  -> ValidateT (NonEmpty Error) m (Set PackageName)
getDependencies opt package =
  CabalFile.externalDependenciesOf package . Set.toList
    <$> resolveComponentSet (componentSetSpecification opt) package

getPackage
  :: Monad m => Input fp m -> Options fp -> ValidateT (NonEmpty Error) m Package
getPackage i opt =
  CabalFile.applyCabalArguments (cabalArguments opt)
    <$> ( readCabalFile i
            =<< validateCabalFileSource i opt
        )

printDependencies :: Set PackageName -> IO ()
printDependencies = traverse_ (Text.putStrLn . CabalFile.toText)

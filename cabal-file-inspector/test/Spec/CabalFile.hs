module Spec.CabalFile where

import Cabal.Compiler
import Cabal.System
import Cabal.Version
import CabalFile
import Control.Monad.Catch qualified as Exception
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Essentials
import GHC.Stack (HasCallStack)
import Test.Hspec (Spec, context, runIO, shouldBe, specify)
import Prelude (IO)

spec :: HasCallStack => Spec
spec = do
  context "Minimal cabal file with a library" do
    cabalFile <-
      runIO $
        read
          $( embedFile
              =<< makeRelativeToProject "test-data/minimal-with-library.cabal"
           )
    specify "Package name" do
      packageName cabalFile `shouldBe` "apple"

    context "With whatever arguments" do
      let package :: Package = applyCabalArguments defaultCabalArguments cabalFile
      specify "Package name" do
        packageName package `shouldBe` "apple"
      specify "Library names" do
        componentsOfType Library package `shouldBe` ["apple"]
      specify "Dependencies" do
        externalDependenciesOf package [mainLibrary package] `shouldBe` mempty

  context "Minimal cabal file with an executable" do
    cabalFile <-
      runIO $
        read
          $( embedFile
              =<< makeRelativeToProject "test-data/minimal-with-executable.cabal"
           )
    specify "Package name" do
      packageName cabalFile `shouldBe` "apple"

    context "With whatever arguments" do
      let package :: Package = applyCabalArguments defaultCabalArguments cabalFile
      specify "Package name" do
        packageName package `shouldBe` "apple"
      specify "Library names" do
        componentsOfType Library package `shouldBe` []
      specify "Dependencies" do
        externalDependenciesOf package [Executable :/ "banana"] `shouldBe` mempty

  context "Package with internal libraries" do
    cabalFile <-
      runIO $
        read
          $( embedFile
              =<< makeRelativeToProject "test-data/with-internal-libraries.cabal"
           )
    let package :: Package = applyCabalArguments defaultCabalArguments cabalFile

    specify "Library names" do
      componentsOfType Library package
        `shouldBe` ["apple", "apple-internal", "internal-test-utilities"]

    context "Dependencies" do
      specify "main library" do
        externalDependenciesOf package [mainLibrary package]
          `shouldBe` ["base", "text"]
      specify "apple-internal" do
        externalDependenciesOf package [Library :/ "apple-internal"]
          `shouldBe` ["text"]
      specify "internal-test-utilities" do
        externalDependenciesOf package [Library :/ "internal-test-utilities"]
          `shouldBe` ["QuickCheck"]
      specify "apple-test" do
        externalDependenciesOf package [Test :/ "apple-test"]
          `shouldBe` ["hspec", "QuickCheck", "base", "text"]

  context "Flags" do
    cabalFile <-
      runIO $
        read
          $( embedFile
              =<< makeRelativeToProject "test-data/with-flags.cabal"
           )

    let configure flags = applyCabalArguments defaultCabalArguments {flags} cabalFile :: Package

    specify "configuration 1" do
      let package = configure [("use-optics", True), ("use-lens", False)]
      externalDependenciesOf package [mainLibrary package]
        `shouldBe` ["base", "optics"]

    specify "configuration 2" do
      let package = configure [("use-optics", False), ("use-lens", True)]
      externalDependenciesOf package [mainLibrary package]
        `shouldBe` ["base", "lens"]

whateverPlatform :: Platform
whateverPlatform = Platform X86_64 Linux

whateverCompiler :: CompilerInfo
whateverCompiler = unknownCompilerInfo (CompilerId GHC nullVersion) NoAbiTag

defaultCabalArguments :: CabalArguments
defaultCabalArguments =
  CabalArguments
    { flags = mempty
    , platform = whateverPlatform
    , compiler = whateverCompiler
    }

read :: ByteString -> IO CabalFile
read =
  either (Exception.throwM . CabalReadException) pure
    . runValidate
    . pedanticCabalRead
    . parseCabalFile

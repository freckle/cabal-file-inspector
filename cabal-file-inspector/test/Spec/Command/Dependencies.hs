module Spec.Command.Dependencies where

import CabalFile (PackageName)
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Function (flip)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Essentials
import Exe.Command.Dependencies (getDependencies, getPackage, optionsParser)
import Exe.Error
import Exe.Input
import GHC.Stack (HasCallStack)
import Map (Map)
import Map qualified
import Set (Set)
import Test.HUnit
import Test.Hspec (Spec, context, shouldBe, specify)
import Testing.Optparse
import Prelude (IO, String, fail, show, words)

spec :: HasCallStack => Spec
spec = do
  context "flags" do
    let input = fakeInput mempty testFiles

    specify "defaults" do
      dependencies <-
        run input $ words "--cabal-file-path=with-flags.cabal --main-library"
      dependencies `shouldBe` ["base", "optics"]

    specify "overriding" do
      dependencies <-
        run input $
          words
            "--cabal-file-path=with-flags.cabal --main-library --flags=+use-lens,-use-optics"
      dependencies `shouldBe` ["base", "lens"]

  context "internal libraries" do
    let input = fakeInput mempty testFiles

    specify "main library" do
      dependencies <-
        run
          input
          (words "--cabal-file-path=with-internal-libraries.cabal --main-library")
      dependencies `shouldBe` ["base", "text"]
    specify "apple-internal" do
      dependencies <-
        run input $
          words
            "--cabal-file-path=with-internal-libraries.cabal --components=lib:apple-internal"
      dependencies `shouldBe` ["text"]
    specify "internal-test-utilities" do
      dependencies <-
        run input $
          words
            "--cabal-file-path=with-internal-libraries.cabal --components=lib:internal-test-utilities"
      dependencies `shouldBe` ["QuickCheck"]
    specify "apple-test" do
      dependencies <-
        run input $
          words
            "--cabal-file-path=with-internal-libraries.cabal --components=test:apple-test"
      dependencies `shouldBe` ["hspec", "QuickCheck", "base", "text"]

run :: Input Text IO -> [String] -> IO (Set PackageName)
run input args = do
  opt <- parse (optionsParser input) args
  package <- throwIfFailure $ getPackage input opt
  throwIfFailure $ getDependencies opt package

throwIfFailure :: ValidateT (NonEmpty Error) IO a -> IO a
throwIfFailure a = either (assertFailure . show . fmap (.text)) pure =<< runValidateT a

testFiles :: Map Text ByteString
testFiles =
  [
    ( "with-internal-libraries.cabal"
    , $( embedFile
          =<< makeRelativeToProject "test-data/with-internal-libraries.cabal"
       )
    )
  ,
    ( "with-flags.cabal"
    , $( embedFile
          =<< makeRelativeToProject "test-data/with-flags.cabal"
       )
    )
  ]

fakeInput :: ByteString -> Map Text ByteString -> Input Text IO
fakeInput stdin files =
  Input
    { textFilePath = Just
    , readFile = flip Map.lookup files >>> maybe (fail "not found") pure
    , doesFileExist = flip Map.member files >>> pure
    , readStandardInput = pure stdin
    }

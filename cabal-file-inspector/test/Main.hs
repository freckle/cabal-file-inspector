module Main (main) where

import GHC.Stack (HasCallStack)
import Spec.CabalFile qualified
import Spec.Command.Dependencies qualified
import Test.Hspec (context, hspec)
import Prelude (IO)

main :: HasCallStack => IO ()
main = hspec do
  context "CabalFile" Spec.CabalFile.spec
  context "Commands" do
    context "dependencies" Spec.Command.Dependencies.spec

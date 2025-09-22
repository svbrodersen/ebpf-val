module Main where

import BoundSpec
import InterpreterSpec
import qualified WorksetSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  _ <- BoundSpec.spec
  _ <- InterpreterSpec.spec
  _ <- WorksetSpec.spec
  pure ()

module Main where

import BoundSpec
import InterpreterSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  _ <- BoundSpec.spec
  _ <- InterpreterSpec.spec
  pure ()

module WorksetSpec where

import Data.Array ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Definitions
import Ebpf.Asm
import Ebpf_cfg
import Test.Hspec
import Workset

allLabels :: CFG -> Set.Set Label
allLabels graph =
  Set.union
    (Set.map (\(l, _, _) -> l) graph)
    (Set.map (\(_, _, l) -> l) graph)

spec :: Spec
spec = do
  describe "Workset Algorithm" $ do
    it "analyzes a simple program" $ do
      let prog =
            [ Binary B64 Mov (Reg 1) (Imm 5)
            , Binary B64 Mov (Reg 2) (Imm 10)
            , Binary B64 Add (Reg 1) (R (Reg 2))
            , Exit
            ]
      let graph = cfg prog
      let labels = Set.toList $ allLabels graph
      let initial_states = Map.fromList [(l, bottomState) | l <- labels]
      let initial_counters = Map.fromList [(l, 0) | l <- labels]
      let final_states = workSetAlgorithm graph initial_states (Set.singleton $ Set.elemAt 0 graph) initial_counters
      let (_, exit) = Map.findMax final_states
      let expectedReg1 = Value (Interval (Val 15) (Val 15))
      (registers exit ! 1) `shouldBe` expectedReg1

    it "analyzes a program with a conditional" $ do
      let prog =
            [ Binary B64 Mov (Reg 1) (Imm 5)
            , Binary B64 Mov (Reg 2) (Imm 10)
            , JCond Jeq (Reg 1) (Imm 5) 1
            , Binary B64 Add (Reg 1) (R (Reg 2))
            , Exit
            ]
      let graph = cfg prog
      let labels = Set.toList $ allLabels graph
      let initial_states = Map.fromList [(l, bottomState) | l <- labels]
      let initial_counters = Map.fromList [(l, 0) | l <- labels]
      let final_states = workSetAlgorithm graph initial_states (Set.singleton $ Set.elemAt 0 graph) initial_counters
      let (_, exit) = Map.findMax final_states
      let expectedReg1 = Value (Interval (Val 5) (Val 5))
      (registers exit ! 1) `shouldBe` expectedReg1

    it "analyzes a program with a loop" $ do
      let prog =
            [ Binary B64 Mov (Reg 1) (Imm 0)
            , Binary B64 Add (Reg 1) (Imm 1)
            , JCond Jlt (Reg 1) (Imm 5) (-2)
            , Exit
            ]
      let graph = cfg prog
      let labels = Set.toList $ allLabels graph
      let initial_states = Map.fromList [(l, bottomState) | l <- labels]
      let initial_counters = Map.fromList [(l, 0) | l <- labels]
      let final_states = workSetAlgorithm graph initial_states (Set.singleton $ Set.elemAt 0 graph) initial_counters
      let (_, exit) = Map.findMax final_states
      let expectedReg1 = Value (Interval (Val 5) (Val 5))
      (registers exit ! 1) `shouldBe` expectedReg1

module InterpreterSpec where

import Data.Array as Array
import Definitions
import Ebpf.Asm
import Ebpf_cfg
import Interpreter
import Test.Hspec

spec :: Spec
spec = do
  describe "Interpreter" $ do
    describe "handleBinary" $ do
      let s = initState
      it "handles Add" $ do
        let s' = s {registers = registers s // [(1, 5), (2, 10)]}
        let result = handleBinary s' (Add, Reg 1, R (Reg 2))
        registers result ! 1 `shouldBe` 15
      it "handles Sub" $ do
        let s' = s {registers = registers s // [(1, 10), (2, 5)]}
        let result = handleBinary s' (Sub, Reg 1, R (Reg 2))
        registers result ! 1 `shouldBe` 5
      it "handles Mov" $ do
        let s' = s {registers = registers s // [(1, 10), (2, 5)]}
        let result = handleBinary s' (Mov, Reg 1, R (Reg 2))
        registers result ! 1 `shouldBe` 5

    describe "handleUnary" $ do
      let s = initState
      it "handles Neg" $ do
        let s' = s {registers = registers s // [(1, 5)]}
        let result = handleUnary s' (Neg, Reg 1)
        registers result ! 1 `shouldBe` fromInteger (-5)

  describe "Interpreter" $ do
    it "handles Assert Jeq with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 0) (Val 100)))]}
          trans = Assert Jeq r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Value (Interval (Val 42) (Val 42))

    it "handles Assert Jne with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 42) (Val 42)))]}
          trans = Assert Jne r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Bottom

    it "handles Assert Jgt with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 0) (Val 100)))]}
          trans = Assert Jgt r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Value (Interval (Val 43) (Val 100))

    it "handles Assert Jge with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 0) (Val 100)))]}
          trans = Assert Jge r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Value (Interval (Val 42) (Val 100))

    it "handles Assert Jlt with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 0) (Val 100)))]}
          trans = Assert Jlt r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Value (Interval (Val 0) (Val 41))

    it "handles Assert Jle with immediate" $ do
      let r1 = Reg 1
          imm = Imm 42
          initial_state = initState {registers = registers initState // [(1, Value (Interval (Val 0) (Val 100)))]}
          trans = Assert Jle r1 imm
          result = handleTrans initial_state trans
      case result of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right new_state -> do
          let new_interval = registers new_state Array.! 1
          new_interval `shouldBe` Value (Interval (Val 0) (Val 42))

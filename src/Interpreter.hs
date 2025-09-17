module Interpreter where

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Definitions
import Ebpf.Asm as Asm
import Ebpf_cfg
import Prelude

handleBinary :: State -> (BinAlu, Reg, RegImm) -> State
handleBinary state (alu, Reg dst, regimm) =
  let dst_val = registers state Array.! dst
      rhs =
        case regimm of
          R (Reg r) -> registers state Array.! r
          Imm i -> fromInteger (fromIntegral i)
      result =
        case alu of
          Add -> addIntervalM dst_val rhs
          Sub -> subIntervalM dst_val rhs
          Mul -> mulIntervalM dst_val rhs
          Div -> divIntervalM dst_val rhs
          Mov -> rhs
          -- Bitwise and other operations are approximated to Top
          Or -> bitWiseIntervalM dst_val rhs
          And -> bitWiseIntervalM dst_val rhs
          Lsh -> bitWiseIntervalM dst_val rhs
          Rsh -> bitWiseIntervalM dst_val rhs
          Mod -> bitWiseIntervalM dst_val rhs
          Xor -> bitWiseIntervalM dst_val rhs
          Arsh -> bitWiseIntervalM dst_val rhs
   in state {registers = registers state // [(dst, result)]}

handleUnary :: State -> (UnAlu, Reg) -> State
handleUnary state (alu, Reg reg) =
  let val = registers state Array.! reg
      result =
        case alu of
          Neg -> negateIntervalM val
          -- Endianness conversions are no-ops for the value analysis
          Le -> val
          Be -> val
   in state {registers = registers state // [(reg, result)]}

handleNonCF :: State -> Instruction -> State
handleNonCF state inst =
  case inst of
    Binary _ alu reg regimm -> handleBinary state (alu, reg, regimm)
    Unary _ alu reg -> handleUnary state (alu, reg)
    Store _ (Reg dst) offset regimm ->
      storeMemory dst offset regimm
    Load _ (Reg dst) (Reg src) offset ->
      loadMemory dst offset (R (Reg src))
    LoadImm (Reg dst) imm ->
      state {registers = registers state // [(dst, fromIntegral imm)]}
    _ -> state
  where
    loadMemory dst Nothing (R (Reg src)) =
      -- If it is a register, then we want to load the memory pointed to by the register, and then update the interval
      let intv = registers state Array.! src
       in case getBounds intv of
            Nothing -> state
            Just (lo', hi') ->
              let newInt = Prelude.foldl unionIntervalM Bottom $ [memory state Array.! i | i <- [lo' .. hi']]
               in state {registers = registers state // [(dst, newInt)]}
    loadMemory dst Nothing (Imm src) =
      -- If we have a memory, then we just load the interval of the memory and set the register
      let newInt = memory state Array.! fromIntegral src
       in state {registers = registers state // [(dst, newInt)]}
    loadMemory dst (Just off) (R (Reg src)) =
      let intv = registers state Array.! src
       in case getBounds (intv - fromIntegral off) of
            Nothing -> state
            Just (lo', hi') ->
              let newInt = Prelude.foldl unionIntervalM Bottom $ [memory state Array.! i | i <- [lo' .. hi']]
               in state {registers = registers state // [(dst, newInt)]}
    loadMemory dst (Just off) (Imm src) =
      let newInt = memory state Array.! (fromIntegral src - fromIntegral off)
       in state {registers = registers state // [(dst, newInt)]}
    storeMemory _ _ _ = state -- For now, we don't model stores
    getBounds (Value (Interval lo hi)) =
      -- Make sure we have some memory we can index
      if lo == PosInf || hi == NegInf
        then
          Nothing
        else
          let lo' = case max lo (Val 0) of
                Val x' -> x'
                _ -> 0
              hi' = case min hi (Val 511) of
                Val x' -> x'
                _ -> 511
           in Just (fromInteger lo', fromInteger hi')
    getBounds Bottom = Nothing

handleTrans :: State -> Trans -> State
handleTrans state (NonCF inst) = handleNonCF state inst
-- If we jump, then we don't know anything new
handleTrans state Unconditional = state
handleTrans state (Assert jmp (Reg lhs) regimm) =
  let lhs_val = registers state Array.! lhs
      rhs_val = case regimm of
        R (Reg r) -> registers state Array.! r
        Imm i -> fromIntegral i
      result = do
        i1 <- lhs_val
        i2 <- rhs_val
        let val =
              case jmp of
                Jeq -> equalInterval i1 i2
                Jne -> notEqualInterval i1 i2
                Jlt -> lessThanInterval i1 i2
                Jle -> lessThanEqualInterval i1 i2
                -- Swap use of lessthan
                Jgt -> greaterThanInterval i1 i2
                Jge -> greaterThanEqualInterval i1 i2
                _ -> Value (i1, i2) -- For other jumps, we don't refine
        let (lhs_new, rhs_new) =
              case val of
                Bottom -> (Bottom, Bottom)
                Value (i1', i2') -> (Value i1', Value i2')
        let new_regs =
              case regimm of
                R (Reg r) -> registers state // [(lhs, lhs_new), (r, rhs_new)]
                Imm _ -> registers state // [(lhs, lhs_new)]
        return state {registers = new_regs}
   in case result of
        Bottom -> state
        Value s -> s

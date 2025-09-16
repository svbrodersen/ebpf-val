{-# LANGUAGE FlexibleInstances #-}

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
   in state{registers = registers state // [(dst, result)]}

handleUnary :: State -> (UnAlu, Reg) -> State
handleUnary state (alu, Reg reg) =
  let val = registers state Array.! reg
      result =
        case alu of
          Neg -> negateIntervalM val
          -- Endianness conversions are no-ops for the value analysis
          Le -> val
          Be -> val
   in state{registers = registers state // [(reg, result)]}

handleNonCF :: State -> Instruction -> Either String State
handleNonCF state inst =
  case inst of
    Binary _ alu reg regimm -> Right $ handleBinary state (alu, reg, regimm)
    Unary _ alu reg -> Right $ handleUnary state (alu, reg)
    Store _ (Reg dst) offset regimm ->
      Right $ storeMemory dst offset regimm
    Load _ (Reg dst) (Reg src) offset ->
      Right $ loadMemory dst offset (R (Reg src))
    LoadImm (Reg dst) imm ->
      Right $ state{registers = registers state // [(dst, fromIntegral imm)]}
    x -> Left $ "Unsupported instruction: " ++ show x
 where
  loadMemory dst Nothing (R (Reg src)) =
    -- If it is a register, then we want to load the memory pointed to by the register, and then update the interval
    let intv = registers state Array.! src
     in case getBounds intv of
          Nothing -> state
          Just (lo', hi') ->
            let newInt = Prelude.foldl unionIntervalM Bottom $ [memory state Array.! i | i <- [lo' .. hi']]
             in state{registers = registers state // [(dst, newInt)]}
  loadMemory dst Nothing (Imm src) =
    -- If we have a memory, then we just load the interval of the memory and set the register
    let newInt = memory state Array.! fromIntegral src
     in state{registers = registers state // [(dst, newInt)]}
  loadMemory dst (Just off) (R (Reg src)) =
    let intv = registers state Array.! src
     in case getBounds (intv - fromIntegral off) of
          Nothing -> state
          Just (lo', hi') ->
            let newInt = Prelude.foldl unionIntervalM Bottom $ [memory state Array.! i | i <- [lo' .. hi']]
             in state{registers = registers state // [(dst, newInt)]}
  loadMemory dst (Just off) (Imm src) =
    let newInt = memory state Array.! (fromIntegral src - fromIntegral off)
     in state{registers = registers state // [(dst, newInt)]}
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

handleCfg :: State -> (Label, Trans, Label) -> CFG -> Either String State
handleCfg state (l1, NonCF inst, l2) _ =
  -- Insert l1 as a dependency of l2
  let newState = state{dependencies = Map.insertWith Set.union l2 (Set.singleton l1) (dependencies state)}
   in handleNonCF newState inst
handleCfg state (l1, Unconditional, l2) graph =
  case filtered of
    [(l2', trans, l3)] ->
      let newState' =
            state
              { dependencies =
                  Map.insertWith
                    Set.union
                    l2
                    (Set.singleton l1)
                    (dependencies state)
              }
          newState =
            newState'
              { dependencies =
                  Map.insertWith
                    Set.union
                    l3
                    (Set.singleton l2)
                    (dependencies newState')
              }
       in handleCfg newState (l2', trans, l3) graph
    _ : _ -> Left $ "Found multiple labels: " ++ show l2 ++ "\n"
    _ -> Left $ "Found no label: " ++ show l2 ++ "\n"
 where
  filtered = Set.toList $ Set.filter (\(from, _, _) -> from == l2) graph
handleCfg state (_, Assert jmp (Reg lhs) regimm, _) _ = Left "Asserts not implemented yet"

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
handleBinary state (alu, Reg v1, regimm) =
  case (alu, regimm) of
    (Add, R (Reg v2)) -> helperReg' state addIntervalM v1 v2
    (Mul, R (Reg v2)) -> helperReg' state mulIntervalM v1 v2
    (Div, R (Reg v2)) -> helperReg' state divIntervalM v1 v2
    (Mov, R (Reg v2)) ->
      let newInt = registers state Array.! v2
       in state{registers = registers state // [(v1, newInt)]}
    -- All the same but with memory instead
    (Add, Imm v2) -> helperMem' state addIntervalM v1 v2
    (Mul, Imm v2) -> helperMem' state mulIntervalM v1 v2
    (Div, Imm v2) -> helperMem' state divIntervalM v1 v2
    (Mov, Imm v2) ->
      let v2' = fromIntegral v2
          newInt = registers state Array.! v2'
       in state{registers = registers state // [(v1, newInt)]}
    (_, R (Reg v2)) -> helperReg' state bitWiseIntervalM v1 v2
    (_, Imm v2) -> helperMem' state bitWiseIntervalM v1 v2
 where
  helperReg' st f v1' v2' =
    let lhs = registers st Array.! v1'
        rhs = registers st Array.! v2'
        u1 = f lhs rhs
     in state{registers = registers st // [(v1, u1)]}
  helperMem' st f v1' v2' =
    let v2'' = fromIntegral v2'
        lhs = memory st Array.! v1'
        rhs = memory st Array.! v2''
        u1 = f lhs rhs
     in state{registers = registers st // [(v1, u1)]}

handleUnary :: State -> (UnAlu, Reg) -> State
handleUnary state (alu, Reg v1) =
  case alu of
    Neg -> helperReg' state negateIntervalM v1
    Le -> helperReg' state negateIntervalM v1
    -- TODO: What is BE???
    Be -> undefined
 where
  helperReg' st f v1' =
    let curInt = registers st Array.! v1'
        newInt = f curInt
     in state{registers = registers st // [(v1', newInt)]}

handleNonCF :: State -> Instruction -> IO State
handleNonCF state inst =
  case inst of
    Binary _ alu reg regimm -> return $ handleBinary state (alu, reg, regimm)
    Unary _ alu reg -> return $ handleUnary state (alu, reg)
    -- TODO: What is the offset used for here?
    Store _ (Reg dst) offset regimm ->
      return $ helperStore dst offset regimm
    Load _ (Reg dst) (Reg src) offset ->
      return $ helperStore dst offset (R (Reg src))
    LoadImm (Reg dst) imm ->
      return $ state{registers = registers state // [(dst, fromIntegral imm)]}
    x -> do
      -- Rest is not handled
      print ("Unsupported item" ++ show x)
      return state
 where
  helperStore dst Nothing (R (Reg src)) =
    -- If it is a register, then we want to load the memory pointed to by the register, and then update the interval
    let intv = registers state Array.! src
     in case getBounds intv of
          Nothing -> state
          Just (lo', hi') ->
            let newInt = Prelude.foldl unionIntervalM topIntervalM $ [memory state Array.! i | i <- [lo' .. hi']]
             in state{registers = registers state // [(dst, newInt)]}
  helperStore dst Nothing (Imm src) =
    -- If we have a memory, then we just load the interval of the memory and set the register
    let newInt = memory state Array.! fromIntegral src
     in state{registers = registers state // [(dst, newInt)]}
  helperStore dst (Just off) (R (Reg src)) =
    let intv = registers state Array.! src
     in case getBounds (intv - fromIntegral off) of
          Nothing -> state
          Just (lo', hi') ->
            let newInt = Prelude.foldl unionIntervalM topIntervalM $ [memory state Array.! i | i <- [lo' .. hi']]
             in state{registers = registers state // [(dst, newInt)]}
  helperStore dst (Just off) (Imm src) =
    let newInt = memory state Array.! (fromIntegral src - fromIntegral off)
     in state{registers = registers state // [(dst, newInt)]}
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

handleCfg :: State -> (Label, Trans, Label) -> CFG -> IO State
handleCfg state (l1, NonCF inst, l2) _ =
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
    _ : _ -> do
      print $ "Found multiple labels: " ++ show l2 ++ "\n"
      return state
    _ -> do
      print $ "Found no label: " ++ show l2 ++ "\n"
      return state
 where
  filtered = Set.toList $ Set.filter (\(from, _, _) -> from == l2) graph
handleCfg state (_, Assert jmp (Reg lhs) regimm, _) _ = undefined

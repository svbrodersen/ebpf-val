module Workset where

import Data.Array as Array
import Data.Map.Strict as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Definitions hiding (unionArray, unionState)
import Ebpf.Display ()
import Ebpf_cfg
import Interpreter

wideningCount :: Int
wideningCount = 7

narrowingCount :: Int
narrowingCount = wideningCount + 7

-- Extends the unionState function to use widening
widenState :: State -> State -> State
widenState s1 s2 =
  State
    { registers = widenArray (registers s1) (registers s2)
    , memory = widenArray (memory s1) (memory s2)
    }
 where
  widenArray a1 a2 =
    let (lo, hi) = bounds a1
     in array (lo, hi) [(i, wideningIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

narrowingState :: State -> State -> State
narrowingState s1 s2 =
  State
    { registers = narrowingArray (registers s1) (registers s2)
    , memory = narrowingArray (memory s1) (memory s2)
    }
 where
  narrowingArray :: Array Int IntervalM -> Array Int IntervalM -> Array Int IntervalM
  narrowingArray a1 a2 =
    let (lo, hi) = bounds a1
     in array (lo, hi) [(i, narrowingIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

unionState :: State -> State -> State
unionState s1 s2 =
  State
    { registers = unionArray (registers s1) (registers s2)
    , memory = unionArray (memory s1) (memory s2)
    }
 where
  unionArray :: Array Int IntervalM -> Array Int IntervalM -> Array Int IntervalM
  unionArray a1 a2 =
    let (lo, hi) = bounds a1
     in array (lo, hi) [(i, unionIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

workSetAlgorithm :: CFG -> Map Label State -> CFG -> Map Label Int -> Map Label State
workSetAlgorithm graph states workset counters
  | Set.null workset = states
  | otherwise =
      let (l, instr, n) = Set.elemAt 0 workset
          w' = Set.deleteAt 0 workset
          current_state = getPreviousState l
          evalState = handleTrans current_state instr
          oldState = states Map.! n
          newState = unionState evalState oldState
       in if oldState == newState
            -- We don't add anything else
            then workSetAlgorithm graph states w' counters
            else
              workSet' w' n newState oldState
 where
  getSuccessors n' = Set.filter (\(l', _, _) -> l' == n') graph
  getPreviousState l = states Map.! l
  workSet' w' n newState oldState =
    -- Check the counter to figure out if we widen or not
    let total_count = counters Map.! n
        successors = getSuccessors n
        newWorkset = Set.union w' successors
        newStates = Map.insert n newState states
        res
          -- Regular work set algorithm, if we have reached the node less than 4 times
          | (total_count < wideningCount) =
              let newCount = Map.insert n (total_count + 1) counters
               in workSetAlgorithm graph newStates newWorkset newCount
          -- Widening if we have been here 4 times.
          | (total_count == wideningCount) =
              let newStates' = Map.insert n (widenState oldState newState) states
                  newCount = Map.insert n (total_count + 1) counters
               in workSetAlgorithm graph newStates' newWorkset newCount
          -- Narrow state until we have reached the node 10 times
          | (wideningCount < total_count) && (total_count < narrowingCount) =
              let newStates' = Map.insert n (narrowingState oldState newState) states
                  newCount = Map.insert n (total_count + 1) counters
               in workSetAlgorithm graph newStates' newWorkset newCount
          | otherwise = workSetAlgorithm graph newStates w' counters
     in res

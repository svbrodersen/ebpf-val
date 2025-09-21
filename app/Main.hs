module Main where

import Data.Array as Array
import Data.Map.Strict as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Definitions hiding (unionArray, unionState)
import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg
import Interpreter
import qualified System.Environment as Sys
import Text.Printf

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
workSetAlgorithm graph states worklist counters
  | Set.null worklist = states
  | otherwise =
      let (l, instr, n) = Set.elemAt 0 worklist
          w' = Set.deleteAt 0 worklist
          current_state = getPreviousState l
          evalState = handleTrans current_state instr
          oldState = states Map.! n
          newState = unionState evalState oldState
          newStates = Map.insert n newState states
          successors = getSuccessors n
       in if oldState == newState
            -- We don't add anything else
            then workSetAlgorithm graph states w' counters
            else
              -- Check the counter to figure out if we widen or not
              let total_count = counters Map.! n
                  newWorkset = Set.union w' successors
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
                    | otherwise = states
               in res
 where
  getSuccessors n' = Set.filter (\(l', _, _) -> l' == n') graph
  getPreviousState l = states Map.! l

allLabels :: CFG -> Set Label
allLabels graph =
  Set.union
    (Set.map (\(l, _, _) -> l) graph)
    (Set.map (\(_, _, l) -> l) graph)

predMap :: CFG -> Map Label [Label]
predMap graph =
  let edges = Set.toList graph
      predList = [(dst, src) | (src, _, dst) <- edges]
   in Map.fromListWith (++) [(dst, [src]) | (dst, src) <- predList]

main :: IO ()
main =
  do
    args <- Sys.getArgs
    case args of
      [ebpfFile, outFile] ->
        do
          res <- parseFromFile ebpfFile
          case res of
            Left err ->
              do
                putStrLn "Some sort of error occurred while parsing:"
                print err
            Right prog ->
              do
                printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
                let graph = cfg prog
                    labels = Set.toList $ allLabels graph
                    initial_states = Map.fromList [(l, bottomState) | l <- labels]
                    initial_counters = Map.fromList [(l, 0) | l <- labels]
                    final_states = workSetAlgorithm graph initial_states (Set.singleton $ Set.elemAt 0 graph) initial_counters
                    (_, exit) = Map.findMax final_states
                    output = printf (printState exit)
                writeFile outFile output
                printf "Wrote analysis results to %s\n" outFile
      _ ->
        putStrLn "Usage <EBPF_FILE> <OUT_FILE>"
 where
  printState st = "Registers: " ++ show (registers st) ++ "\n\nMemory: " ++ show (memory st) ++ "\n"

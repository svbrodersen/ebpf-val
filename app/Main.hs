module Main where

import Data.Array as Array
import Data.Map.Strict as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Debug.Trace
import Definitions hiding (unionArray, unionState)
import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg
import Interpreter
import qualified System.Environment as Sys
import Text.Printf

-- Extends the unionState function to use widening
widenState :: State -> State -> State
widenState s1 s2 =
  State
    { registers = widenArray (registers s1) (registers s2),
      memory = widenArray (memory s1) (memory s2)
    }
  where
    widenArray a1 a2 =
      let (lo, hi) = bounds a1
       in array (lo, hi) [(i, wideningIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

narrowingState :: State -> State -> State
narrowingState s1 s2 =
  State
    { registers = narrowingArray (registers s1) (registers s2),
      memory = narrowingArray (memory s1) (memory s2)
    }
  where
    narrowingArray :: Array Int IntervalM -> Array Int IntervalM -> Array Int IntervalM
    narrowingArray a1 a2 =
      let (lo, hi) = bounds a1
       in array (lo, hi) [(i, narrowingIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

unionState :: State -> State -> State
unionState s1 s2 =
  State
    { registers = unionArray (registers s1) (registers s2),
      memory = unionArray (memory s1) (memory s2)
    }
  where
    unionArray :: Array Int IntervalM -> Array Int IntervalM -> Array Int IntervalM
    unionArray a1 a2 =
      let (lo, hi) = bounds a1
       in array (lo, hi) [(i, unionIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

workSetAlgorithm :: CFG -> Map Label Trans -> Map Label [Label] -> Map Label State -> Set Label -> Map Label State
workSetAlgorithm graph instrMap prevMap states worklist
  | Set.null worklist = states
  | otherwise =
      let l = Set.elemAt 0 worklist
          current_state = trace ("Get State: " ++ show l) $ getCurrentState l
          w' = Set.deleteAt 0 worklist
          successors = [dst | (src, _, dst) <- Set.toList graph, src == l]
          newState =
            case getInstr l of
              Just instr -> handleTrans current_state instr
              Nothing -> trace ("Got nothing for: " ++ show l) current_state
       in if current_state == newState
            -- We don't add anything else
            then workSetAlgorithm graph instrMap prevMap states w'
            -- Here we add all the elements that depend on us
            else
              let newStates = getNewState successors newState
               in workSetAlgorithm graph instrMap prevMap newStates (Set.union w' (Set.fromList successors))
  where
    -- TODO: Issue is that if we want to update the next state, then we should overwrite the state, unless there are multiple ways to get there.
    -- Have to change to instead look for the predecesor instead of trying to update the successor. Then also have to update the current label in the states, whenever we have finished an instruction.
    getNewState successors st' = foldl (\state n -> Map.insertWith unionState n st' state) states successors
    getCurrentState l' = states Map.! l'
    getInstr l =
      Map.lookup l instrMap

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
                    labels = allLabels graph
                    initial_states = Map.fromList [(l, initState) | l <- Set.toList labels]
                    instrMap = buildInstrMap graph
                    prevMap = predMap graph
                    final_states = workSetAlgorithm graph instrMap prevMap initial_states labels
                    output = printf ("States: " ++ show (Map.toList final_states))
                writeFile outFile output
                printf "Wrote analysis results to %s\n" outFile
      _ ->
        putStrLn "Usage <EBPF_FILE> <OUT_FILE>"
  where
    buildInstrMap graph =
      Map.fromList [(src, instr) | (src, instr, _) <- Set.toList graph]

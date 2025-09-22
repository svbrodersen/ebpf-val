module Main where

import Data.Map.Strict as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Definitions hiding (unionArray, unionState)
import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg
import qualified System.Environment as Sys
import Text.Printf
import Workset

allLabels :: CFG -> Set Label
allLabels graph =
  Set.union
    (Set.map (\(l, _, _) -> l) graph)
    (Set.map (\(_, _, l) -> l) graph)

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

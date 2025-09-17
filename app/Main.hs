{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map as Map
import Data.Set as Set
import Definitions (State (dependencies, memory, registers), initState)
import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg
import Interpreter
import qualified System.Environment as Sys
import Text.Printf

workSetAlgorithm :: CFG -> CFG -> State -> String
workSetAlgorithm graph w tmp =
  let i = Set.elemAt 0 w
      res = handleInstruction tmp i
   in case res of
        Left str -> str
        Right st -> if st == tmp then stateToString st else recurse i st
  where
    stateToString st' = printf "registers: %s\nMemory: %s\n" (show $ registers st') (show $ memory st')
    recurse i st =
      let depends =
            case i of
              (x, _, _) -> getDependencies x st
          w' = Set.deleteAt 0 w
       in workSetAlgorithm graph (Set.union w' depends) st
    getDependencies l st =
      let labels = Map.lookup l (dependencies st)
       in case labels of
            Nothing -> Set.empty
            (Just ls) -> Set.fromList $ Set.toList ls >>= (\x -> Set.toList $ Set.filter (\(y, _, _) -> x == y) graph)

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, outFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
          let graph = cfg prog
              output = workSetAlgorithm graph graph initState
          writeFile outFile output
          printf "Visualised the CFG in %s\n" outFile
    _ ->
      putStrLn "Usage <EBPF_FILE>"

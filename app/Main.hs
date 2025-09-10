
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()
import qualified System.Environment as Sys
import Text.Printf
import Ebpf.cfg (cfg, cfgToDot, dotPrelude, markNodes)

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, dotFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
          let edges = cfgToDot $ cfg prog
          writeFile
            dotFile
            ( dotPrelude
                ++ edges
                ++ markNodes prog
                ++ "}"
            )
          printf "Visualised the CFG in %s\n" dotFile
    _ ->
      putStrLn "Usage <EBPF_FILE>"

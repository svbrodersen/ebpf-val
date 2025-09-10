{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg (cfg, cfgToDot, dotPrelude, markNodes)
import qualified System.Environment as Sys
import Text.Printf

type Mem = [Int]

data State = State
  { registers :: [Reg]
  , memory :: Mem
  }
  deriving (Show, Eq)

data Exp
  = EReg Reg
  | EConst Int
  | EMem Mem Reg
  | EBinOp BinAlu Exp Exp

data ConditionalExpression
  = Equal Exp Exp
  | NotEqual Exp Exp
  | LessThan Exp Exp
  | LessThanEqual Exp Exp

type Label = String

data Statement
  = AssignReg Reg Exp
  | AssignMem Mem Reg Reg
  | If ConditionalExpression Label
  | Goto Label

type LabelledStatement = (Label, Statement)

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

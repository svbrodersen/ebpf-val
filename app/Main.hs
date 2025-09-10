{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ebpf.AsmParser
import Ebpf.Display ()
import Ebpf_cfg (cfg, cfgToDot, dotPrelude, markNodes)
import qualified System.Environment as Sys
import Text.Printf

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Show, Eq, Enum, Bounded)

type Mem = [Int]

data State = State
  { registers :: [(Register, Int)]
  , memory :: Mem
  }
  deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

data Exp
  = EReg Register
  | EConst Int
  | EMem Mem Register
  | EBinOp BinOp Exp Exp

data ConditionalExpression
  = Equal Exp Exp
  | NotEqual Exp Exp
  | LessThan Exp Exp
  | LessThanEqal Exp Exp

type Label = String

data Statement
  = AssignReg Register Exp
  | AssignMem Mem Register Register
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

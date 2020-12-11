----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates the specification that is realizable with a minimal amount of 
-- assumptions
--
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------
import TSL (toTSL)

import TSLCoreGenerator
  ( generateMinimalAssumptions
  , treeBasedMinimalAssumptions
  )

import System.Console.ANSI (Color(..))
import System.Environment (getArgs)

import Utils

-----------------------------------------------------------------------------
helpMessage :: [String]
helpMessage =
  [ "tslminrealizable [--verbose [0 | 1 | 2 | 3]] [--tree-based] <POOL SIZE> <REALIZABLE call> <file>"
  , "  <REALIZABLE call>: a path to an executable taking a TLSF file on STDIN and outputting \"REALIZABLE\" or \"UNREALIZABLE\""
  , "   Verbosity:"
  , "    0: silent, only output the result"
  , "    1: quiet, only output the result and important information (default)"
  , "    2: output which intermediate steps are reached"
  , "    3: output all intermediate steps including the specifications"
  ]

-----------------------------------------------------------------------------
main :: IO ()
main = do
  initEncoding
  args <- getArgs
  (verbosityStr, treeBased, poolSizeStr, realizableCommand, filepath) <-
    case args of
      ["--verbose", num, s, a, c] -> return (num, False, s, a, c)
      ["--verbose", num, "--tree-based", s, a, c] -> return (num, True, s, a, c)
      ["--tree-based", "--verbose", num, s, a, c] -> return (num, True, s, a, c)
      ["--tree-based", s, a, c] -> return ("1", True, s, a, c)
      [s, a, c] -> return ("1", False, s, a, c)
      _ -> printHelpAndExit helpMessage
  verbosity <- parseVerbosity verbosityStr
  poolSize <- parsePoolSize poolSizeStr
  spec <- tryLoadTSL filepath
  potMinimalRealizableSpec <-
    (if treeBased
       then treeBasedMinimalAssumptions
       else generateMinimalAssumptions)
      (createContext poolSize verbosity realizableCommand)
      spec
  case potMinimalRealizableSpec of
    Nothing -> cPutStrLn Red "UNREALIZABLE"
    Just core -> putStr $ toTSL core
  resetColors
-----------------------------------------------------------------------------

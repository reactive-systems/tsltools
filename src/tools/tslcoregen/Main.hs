----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates a core for a unrealizable specification
--
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL (toTSL)

import TSLCoreGenerator (generateCore)

import System.Environment (getArgs)

import Utils

-----------------------------------------------------------------------------
helpMessage :: [String]
helpMessage =
  [ "tslcoregen [--verbose [0 | 1 | 2 | 3]] <POOL SIZE> <REALIZABLE call> <file>"
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
  (verbosityStr, poolSizeStr, realizableCommand, filepath) <-
    case args of
      ["--verbose", num, s, a, b] -> return (num, s, a, b)
      [s, a, b] -> return ("1", s, a, b)
      _ -> printHelpAndExit helpMessage
  verbosity <- parseVerbosity verbosityStr
  poolSize <- parsePoolSize poolSizeStr
  spec <- tryLoadTSL filepath
  potCore <-
    generateCore (createContext poolSize verbosity realizableCommand) spec
  case potCore of
    Nothing -> cPutOutLn Vivid Red "Specification is realizable"
    Just core -> do
      putStrLn "UNREALIZBALE CORE"
      putStr $ toTSL core
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- |
-- Module      :  External.ToolCalls
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generate Tool Context out of Tools
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module External.ToolCalls where

-----------------------------------------------------------------------------
import External.Context (Context(..))
import TSL.Aiger (Circuit)

import System.IO
import System.Process

import Data.List

-----------------------------------------------------------------------------
--
-- Executes a given command with an input given to stdin and return 
-- stdout and stderr
--
execCMD :: String -> String -> IO (String, String)
execCMD cmd stdIn = do
  (i, o, e, p) <- runInteractiveCommand cmd
  hSetBuffering o NoBuffering
  hSetBuffering e NoBuffering
  hPutStr i stdIn
  hClose i
  sout <- hGetContents o
  serr <- hGetContents e
  _ <- length sout `seq` waitForProcess p
  return (sout, serr)

-----------------------------------------------------------------------------
--
-- Implements a sat check, by calling trp++uc
--
satTrpppuc :: FilePath -> FilePath -> String -> IO Bool
satTrpppuc trpppuc syfco tlsf = do
  (ltl, _) <- execCMD (syfco ++ " -f trp -in") tlsf
  writeFile "tmp133742.tmp" ltl
  (out, _) <- execCMD (trpppuc ++ " -f ltl -q tmp133742.tmp") ""
  _ <- execCMD "rm tmp133742.tmp" ""
  return (not $ isInfixOf "Unsatisfiable" out)

-----------------------------------------------------------------------------
--
-- Calls the strix synthesizer tool given the base directory of the strix
-- binary and owl.jar. Note that an addition argument tells if a result should be
-- synthesized
--
strix :: String -> Bool -> String -> IO String
strix strixDir synth tlsf = do
  (ltl, _) <- execCMD "syfco -f rabinizer -in" tlsf
  (inp, _) <- execCMD "syfco -f rabinizer --print-input-signals -in" tlsf
  (out, _) <- execCMD "syfco -f rabinizer --print-output-signals -in" tlsf
  (str, _) <-
    execCMD
      (strixDir ++
       "/strix --owl-jar-dir " ++
       strixDir ++
       (if synth
          then ""
          else " -r ") ++
       " -f \"" ++ ltl ++ "\" --ins \"" ++ inp ++ "\" --outs \"" ++ out ++ "\"")
      ""
  return str

-----------------------------------------------------------------------------
--
-- Implements a realizabilty check, by calling strix
--
realStrix :: FilePath -> FilePath -> String -> IO Bool
realStrix strixPath syfco tlsf = do
  str <- strix "~/tools/strix/bin" False tlsf
  return (str == "REALIZABLE\n")

-----------------------------------------------------------------------------
--
-- Implements synthesis, by calling strix
--
syntStrix :: FilePath -> FilePath -> String -> IO (Maybe Circuit)
syntStrix strixPath syfco tlsf = error "TODO"

-----------------------------------------------------------------------------
--
-- Implements counter startegy synthesis, by calling strix
--
counterSyntStrix :: FilePath -> FilePath -> String -> IO (Maybe Circuit)
counterSyntStrix strixPath syfco tlsf = error "TODO"

-----------------------------------------------------------------------------
--
-- Generate context with strix
--
data StrixPath =
  StrixPath
    { strixPath :: String
    , syfco :: String
    , trpppuc :: String
    }

genStrixContext :: StrixPath -> Context
genStrixContext StrixPath {..} =
  Context
    { sat = satTrpppuc trpppuc syfco
    , real = realStrix strixPath syfco
    , synt = syntStrix strixPath syfco
    , counterSynt = counterSyntStrix strixPath syfco
    }

-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen.ToolCall
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Uses generated core canidates and call a synthesizer to generate 
-- a core
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module CoreGen.ToolCall
  ( generateCore
  , generateCoreFromFile
  ) where

-----------------------------------------------------------------------------
import CoreGen.CoreGen (Query(..), genQuery, getCores)
import TSL.Error
import TSL.Reader (fromTSLtoTSLSpec)
import TSL.Specification (TSLSpecification)
import TSL.ToString (tslSpecToString)

import System.Exit
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
  length sout `seq` waitForProcess p
  return (sout, serr)

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
realizable :: String -> IO Bool
realizable tlsf = do
  str <- strix "~/tools/strix/bin" False tlsf
  return (str == "REALIZABLE\n")

-----------------------------------------------------------------------------
--
-- Implements a sat check, by calling trp++uc
--
satisfiable :: String -> IO Bool
satisfiable tlsf = do
  (ltl, _) <- execCMD "syfco -f trp -in" tlsf
  writeFile "tmp133742.tmp" ltl
  (out, _) <- execCMD "trp++uc -f ltl -q tmp133742.tmp" ""
  execCMD "rm tmp133742.tmp" ""
  return (not $ isInfixOf "Unsatisfiable" out)

-----------------------------------------------------------------------------
--
--Notion of diiferent core types
-- 
data Core
  = NaC
  | Unsat TSLSpecification
  | Unrez TSLSpecification

-----------------------------------------------------------------------------
--
-- Generates (eventually) a core given a TSL Specification using a simple
-- realizabilty test
--
generateCore :: TSLSpecification -> IO Core
generateCore tsl = do
  let queries = getCores tsl
  genCore' queries
  where
    genCore' :: [Query] -> IO Core
    genCore' [] = return NaC
    genCore' (q:qr) = do
      sat <- satisfiable $ synthSpec q
      if not sat
        then return (Unsat $ potCore q)
        else do
          rel <- realizable $ synthSpec q
          if rel
            then return (Unrez $ potCore q)
            else do
              genCore' qr

-----------------------------------------------------------------------------
--
-- Wraps the generate core Method into File IO
--
generateCoreFromFile :: FilePath -> IO ()
generateCoreFromFile path = do
  tsl <- readFile path
  case fromTSLtoTSLSpec tsl of
    Left err -> putStrLn (show err)
    Right spec -> do
      core <- generateCore spec
      case core of
        NaC -> print "Not unrealizable"
        Unrez s -> putStrLn ("UNREALIZABLE\n\n" ++ tslSpecToString s)
        Unsat s -> putStrLn ("UNSATISFIABLE\n\n" ++ tslSpecToString s)

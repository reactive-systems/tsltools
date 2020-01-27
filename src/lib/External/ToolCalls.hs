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
import TSL.Aiger (Circuit, parseAag)

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
satTrpppuc :: String -> IO Bool
satTrpppuc tlsf = do
  (ltl, _) <- execCMD ("syfco -f trp -in") tlsf
  writeFile "tmp133742.tmp" ltl
  (out, _) <- execCMD ("trp++uc -f ltl -q tmp133742.tmp") ""
  _ <- execCMD "rm tmp133742.tmp" ""
  return (not $ isInfixOf "Unsatisfiable" out)

-----------------------------------------------------------------------------
--
-- Calls the strix synthesizer tool given the base directory of the strix
-- binary and owl.jar. Note that an addition argument tells if a result should be
-- synthesized
--
strix :: Bool -> String -> IO String
strix synth tlsf = do
  writeFile "tmp133742.tmp" tlsf
  (str, _) <-
    execCMD
      ("~/tools/dockerimagegen/strix/strix tmp133742.tmp" ++
       if synth
         then ""
         else " -r ")
      ""
  _ <- execCMD "rm tmp133742.tmp" ""
  return str

-----------------------------------------------------------------------------
--
-- Implements a realizabilty check, by calling strix
--
realStrix :: String -> IO Bool
realStrix tlsf = do
  str <- strix False tlsf
  return (str == "REALIZABLE\n")

-----------------------------------------------------------------------------
--
-- Implements full synthesis (startgegy or counter strategy), by calling strix
--
fullSyntStrix :: String -> IO (Bool, Circuit)
fullSyntStrix tlsf = do
  out <- strix True tlsf
  case out of
    'R':'E':'A':'L':'I':'Z':'A':'B':'L':'E':'\n':aag ->
      case parseAag aag of
        Left err -> error $ (show err)
        Right cir -> return (True, cir)
    'U':'N':'R':'E':'A':'L':'I':'Z':'A':'B':'L':'E':'\n':aag ->
      case parseAag aag of
        Left err -> error $ (show err)
        Right cir -> return (False, cir)
    _ -> error "Strix Synthesis: Strix outputed some weird stuff"

-----------------------------------------------------------------------------
--
-- Context with strix,
--
-- ASSUMPTION: strix, syfco, trp++uc are inside the path
--
strixContext :: Context
strixContext =
  Context
    { sat = satTrpppuc
    , real = realStrix
    , synt =
        \s -> do
          full <- fullSyntStrix s
          case full of
            (True, cir) -> return $ Just cir
            (False, _) -> return $ Nothing
    , counterSynt =
        \s -> do
          full <- fullSyntStrix s
          case full of
            (False, cir) -> return $ Just cir
            (True, _) -> return $ Nothing
    }

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.InterfacePrintingUtils.hs
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Utilities to print different things for the simulation interfaces
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.InterfacePrintingUtils
  ( updateToString
  , updateListToString
  , predicateEvaluationToString
  , predicateEvaluationListToString
  , mappedPredicateEvaluationToString
  , initInterface
  , resetInterface
  , cPutStr
  , cPutStrLn
  , Color(..)
  ) where

-----------------------------------------------------------------------------
import TSL.Logic (PredicateTerm, SignalTerm)

import TSL.ToString (predicateTermToString, signalTermToString)

import Data.Set as Set (Set, toList)

import GHC.IO.Encoding
  ( setFileSystemEncoding
  , setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , clearScreen
  , setSGR
  )

-----------------------------------------------------------------------------
-- | TODO
updateToString :: (String, SignalTerm String) -> String
updateToString (c, st) = "[" ++ c ++ " <- " ++ signalTermToString id st ++ "]"

-----------------------------------------------------------------------------
-- | TODO
updateListToString :: [(String, SignalTerm String)] -> String
updateListToString [] = ""
updateListToString [x] = updateToString x
updateListToString (x:xr) = updateToString x ++ " " ++ updateListToString xr

-----------------------------------------------------------------------------
-- | TODO
predicateEvaluationToString :: (PredicateTerm String, Bool) -> String
predicateEvaluationToString (pt, v) =
  (if v
     then ""
     else "¬ ") ++
  predicateTermToString id pt

-----------------------------------------------------------------------------
-- | TODO
predicateEvaluationListToString :: [(PredicateTerm String, Bool)] -> String
predicateEvaluationListToString [] = ""
predicateEvaluationListToString [x] = predicateEvaluationToString x
predicateEvaluationListToString (x:xr) =
  predicateEvaluationToString x ++ ", " ++ predicateEvaluationListToString xr

-----------------------------------------------------------------------------
-- | TODO
mappedPredicateEvaluationToString ::
     (Set (PredicateTerm String), PredicateTerm String -> Bool) -> String
mappedPredicateEvaluationToString (terms, mapping) =
  predicateEvaluationListToString (fmap (\p -> (p, mapping p)) $ toList terms)

-----------------------------------------------------------------------------
-- | TODO
initInterface :: IO ()
initInterface = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  return ()

-----------------------------------------------------------------------------
-- | TODO
resetInterface :: IO ()
resetInterface = do
  clearScreen
  putStrLn ""

-----------------------------------------------------------------------------
-- | TODO
cPutStrLn :: Color -> String -> IO ()
cPutStrLn c str = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn str
  setSGR []

-----------------------------------------------------------------------------
-- | TODO
cPutStr :: Color -> String -> IO ()
cPutStr c str = do
  setSGR [SetColor Foreground Vivid c]
  putStr str
  setSGR []

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
-- | Transforms an update to a string
updateToString :: (String, SignalTerm String) -> String
updateToString (c, st) = "[" ++ c ++ " <- " ++ signalTermToString id st ++ "]"

-----------------------------------------------------------------------------
-- | Transforms an update list to a string
updateListToString :: [(String, SignalTerm String)] -> String
updateListToString [] = ""
updateListToString [x] = updateToString x
updateListToString (x:xr) = updateToString x ++ " " ++ updateListToString xr

-----------------------------------------------------------------------------
-- | Transforms a predicate evalutation to a string
predicateEvaluationToString :: (PredicateTerm String, Bool) -> String
predicateEvaluationToString (pt, v) =
  (if v
     then ""
     else "¬ ") ++
  predicateTermToString id pt

-----------------------------------------------------------------------------
-- | Transforms predicate evalutations (as list) to a string
predicateEvaluationListToString :: [(PredicateTerm String, Bool)] -> String
predicateEvaluationListToString [] = ""
predicateEvaluationListToString [x] = predicateEvaluationToString x
predicateEvaluationListToString (x:xr) =
  predicateEvaluationToString x ++ ", " ++ predicateEvaluationListToString xr

-----------------------------------------------------------------------------
-- | Transforms predicate evalutations (as mapping) to a string
mappedPredicateEvaluationToString ::
     (Set (PredicateTerm String), PredicateTerm String -> Bool) -> String
mappedPredicateEvaluationToString (terms, mapping) =
  predicateEvaluationListToString (fmap (\p -> (p, mapping p)) $ toList terms)

-----------------------------------------------------------------------------
-- | Inits the (colored) interface by setting the encoding
initInterface :: IO ()
initInterface = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  return ()

-----------------------------------------------------------------------------
-- | Resets the (colored) interface by clearing the scree
resetInterface :: IO ()
resetInterface = do
  clearScreen
  putStrLn ""

-----------------------------------------------------------------------------
-- | Puts a colored string to stdin (with a newline at the end)
cPutStrLn :: Color -> String -> IO ()
cPutStrLn c str = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn str
  setSGR []

-----------------------------------------------------------------------------
-- | Puts a colored string to stdin
cPutStr :: Color -> String -> IO ()
cPutStr c str = do
  setSGR [SetColor Foreground Vivid c]
  putStr str
  setSGR []

-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.InterfacePrintingUtils.hs
-- Description :  Utilities for the simulation interfaces
-- Maintainer  :  Philippe Heim
--
-- This module provides utilities for different printing operation that are
-- commonly used by the simulation interfaces.
--
-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
  , PredicateTerm
  , SignalTerm
  , tslFormula
  )

import Data.Set
  ( Set
  , toList
  )

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

-------------------------------------------------------------------------------
-- | 'updateToString' prints an update as a string.

updateToString :: (String, SignalTerm String) -> String
updateToString =
  tslFormula id . uncurry Update

-------------------------------------------------------------------------------
-- | 'updateListToString' prints a list of updates as a single string
updateListToString :: [(String, SignalTerm String)] -> String
updateListToString = \case
  []   -> ""
  [x]  -> updateToString x
  x:xr -> updateToString x ++ " " ++ updateListToString xr

-------------------------------------------------------------------------------
-- | 'predicateEvaluationToString' prints a predicate term evaluation as a
-- string in a easily understandable manner.

predicateEvaluationToString :: (PredicateTerm String, Bool) -> String
predicateEvaluationToString (p, v)
  | v         = tslFormula id $ Check p
  | otherwise = "¬ " ++ tslFormula id (Check p)

-------------------------------------------------------------------------------
-- | 'predicateEvaluationListToString' prints a predicate term evaluation list
-- nicely as a string.

predicateEvaluationListToString :: [(PredicateTerm String, Bool)] -> String
predicateEvaluationListToString = \case
  []     -> ""
  [x]    -> predicateEvaluationToString x
  (x:xr) -> predicateEvaluationToString x ++ ", " ++
           predicateEvaluationListToString xr

-------------------------------------------------------------------------------
-- | 'mappedPredicateEvaluationToString' prints the evaluation of a set of
-- predicates as a nice string.
mappedPredicateEvaluationToString
  :: (Set (PredicateTerm String), PredicateTerm String -> Bool) -> String
mappedPredicateEvaluationToString (terms, mapping) =
  predicateEvaluationListToString
    $ (\p -> (p, mapping p)) <$> toList terms

-------------------------------------------------------------------------------
-- | 'initInterface' initializes the simulation interface by setting an
-- appropriate encoding.

initInterface :: IO ()
initInterface = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  return ()

-------------------------------------------------------------------------------
-- | 'resetInterface' resets the simulation interface and clears the screen.

resetInterface :: IO ()
resetInterface = do
  clearScreen
  putStrLn ""

-------------------------------------------------------------------------------
-- | 'cPutStrLn' prints a colored line to STDOUT.
cPutStrLn :: Color -> String -> IO ()

cPutStrLn c str = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn str
  setSGR []

-------------------------------------------------------------------------------
-- | 'cPutStrLn' prints a colored string to STDOUT.

cPutStr :: Color -> String -> IO ()

cPutStr c str = do
  setSGR [SetColor Foreground Vivid c]
  putStr str
  setSGR []

-------------------------------------------------------------------------------

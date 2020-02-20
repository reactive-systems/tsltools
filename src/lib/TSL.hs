-----------------------------------------------------------------------------
-- |
-- Module      :  TSL
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- TSL tools library interface.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module TSL
    -- * Data Structures
  ( Error
  , Formula
  , Specification(..)
  , TSLSpecification(..)
  , CodeTarget(..)
  , CFM
  , SymbolTable
  , ModuleName
  , FunctionName
  , Circuit
    -- * Formula Utilities
  , getOutputs
  , getPossibleUpdates
  , getUpdates
  , constantTrue
  , constantFalse
  , conjunctFormulas
  , disjunctFormulas
  , negateFormula
    -- * TSL Utilties
  , fromTSL
  , fromTSLtoTSLSpec
  , tslSpecToSpec
  , tslSize
  , tslSpecToString
  , st2csv
  , toTLSF
  , splitIgnoreAssumptions
  , split
    -- * CFM Utilities  
  , fromCFM
  , statistics
  , csvSymbolTable
  , implement
    -- * Aiger Utilities
  , parseAag
    -- * Simulator Backend
  , simulate
  ) where

-----------------------------------------------------------------------------
import TSL.Logic (Formula, tslSize)

import TSL.FormulaUtils
  ( conjunctFormulas
  , constantFalse
  , constantTrue
  , disjunctFormulas
  , getOutputs
  , getPossibleUpdates
  , getUpdates
  , negateFormula
  )

import TSL.Error (Error)

import TSL.SymbolTable (SymbolTable, st2csv)

import TSL.Specification
  ( Specification(..)
  , TSLSpecification(..)
  , tslSpecToSpec
  )

import TSL.ToString (tslSpecToString)

import TSL.Splitter (split, splitIgnoreAssumptions)

import TSL.Reader (fromTSL, fromTSLtoTSLSpec)

import TSL.Aiger (Circuit, parseAag)

import TSL.Simulation (simulate)

import TSL.TLSF (toTLSF)

import TSL.CFM (CFM, csvSymbolTable, fromCFM, statistics)

import qualified TSL.Writer.Clash as Clash (implement)

import qualified TSL.Writer.Applicative as Applicative (implement)

import qualified TSL.Writer.Arrow as Arrow (implement)

import qualified TSL.Writer.Monadic as Monadic (implement)

-----------------------------------------------------------------------------
data CodeTarget
  = Applicative
  | Monadic
  | Arrow
  | Clash
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------
type ModuleName = String

type FunctionName = String

-----------------------------------------------------------------------------
-- | Generates code for a specific target from a CFM. The function
-- uses the given module name to generate a module that exports a
-- single function with the given function name.
implement :: CodeTarget -> ModuleName -> FunctionName -> CFM -> String
implement =
  \case
    Applicative -> Applicative.implement
    Arrow -> Arrow.implement
    Clash -> Clash.implement
    Monadic -> Monadic.implement
-----------------------------------------------------------------------------

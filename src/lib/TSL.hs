-----------------------------------------------------------------------------
-- |
-- Module      :  TSL
-- Maintainer  :  Felix Klein
--                Philippe Heim
--                Gideon Geier
--                Marvin Stenger
--
--
-- TSL tools library interface.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL
  ( -- * Formula Structure
    Formula(..)
  , SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , updates
  , checks
  , inputs
  , outputs
  , functions
  , predicates
  , encodeInputAP
  , encodeOutputAP
  , decodeInputAP
  , decodeOutputAP
  , size
    -- * TSL Utilities
  , DependencyRepresentation(..)
  , Specification(..)
  , fromTSL
  , tslFormula
  , toFormula
  , toTSL
  , toTLSF
  , toTOML
  , split
  , specifications2dependencies
    -- * CFM Utilities
  , CodeTarget(..)
  , CFM
  , ModuleName
  , FunctionName
  , fromCFM
  , statistics
  , symbolTable
  , implement
    -- * Symbol Table
  , SymbolTable
  , Kind(..)
  , stName
  , stArgs
  , stDeps
  , stKind
  , toCSV
    -- * Simulation
  , simulate
    -- * Error Handling
  , Error
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  , checks
  , decodeInputAP
  , decodeOutputAP
  , encodeInputAP
  , encodeOutputAP
  , functions
  , inputs
  , outputs
  , predicates
  , size
  , tslFormula
  , updates
  )

import TSL.Error (Error)

import TSL.SymbolTable (Kind(..), SymbolTable(..), toCSV)

import TSL.Specification (Specification(..), toFormula, toTSL)

import TSL.Reader (fromTSL)

import TSL.TLSF (toTLSF)

import TSL.Splitter (split)

import TSL.Dependency
  ( DependencyRepresentation(..)
  , specifications2dependencies
  )

import TSL.Simulation (simulate)

import TSL.TOML (toTOML)

import TSL.CFM (CFM, fromCFM, statistics, symbolTable)

import qualified TSL.Writer.Clash as Clash (implement)

import qualified TSL.Writer.Applicative as Applicative (implement)

import qualified TSL.Writer.Arrow as Arrow (implement)

import qualified TSL.Writer.Monadic as Monadic (implement)

import qualified TSL.Writer.JavaScript as JavaScript
  ( implement
  )

import qualified TSL.Writer.WebAudio as WebAudio
  ( implement
  )

-----------------------------------------------------------------------------

data CodeTarget
  = Applicative
  | Monadic
  | Arrow
  | Clash
  | JavaScript
  | WebAudio
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------

type ModuleName = String
type FunctionName = String

-----------------------------------------------------------------------------

-- | Generates code for a specific target from a CFM. The function
-- uses the given module name to generate a module that exports a
-- single function with the given function name.

implement
  :: CodeTarget -> ModuleName -> FunctionName -> CFM -> String

implement = \case
  Applicative -> Applicative.implement
  Arrow       -> Arrow.implement
  Clash       -> Clash.implement
  Monadic     -> Monadic.implement
  JavaScript  -> JavaScript.implement
  WebAudio    -> WebAudio.implement

-----------------------------------------------------------------------------

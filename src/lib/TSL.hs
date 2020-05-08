-----------------------------------------------------------------------------
-- |
-- Module      :  TSL
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- TSL tools library interface.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module TSL
  ( -- * Data Structures
    Error
  , Formula(..)
  , SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , Specification(..)
  , CodeTarget(..)
  , CFM
  , SymbolTable
  , ModuleName
  , FunctionName
    -- * Formula Utilities
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
    -- * TSL Utilties
  , size
  , toTSL
  , fromTSL
  , st2csv
  , toTLSF
  , split
  , splitIgnoreAssumptions
    -- * CFM Utilities
  , fromCFM
  , statistics
  , symbolTable
  , implement
    -- * Simulation
  , simulate
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
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
  )

import TSL.Error
  ( Error
  )

import TSL.SymbolTable
  ( SymbolTable
  , st2csv
  )

import TSL.Specification
  ( Specification(..)
  , toTSL
  )

import TSL.Reader
  ( fromTSL
  )

import TSL.TLSF
  ( toTLSF
  )

import TSL.Splitter
  ( split
  , splitIgnoreAssumptions
  )

import TSL.Simulation
  ( simulate
  )

import TSL.CFM
  ( CFM
  , symbolTable
  , fromCFM
  , statistics
  )

import qualified TSL.Writer.Clash as Clash
  ( implement
  )

import qualified TSL.Writer.Applicative as Applicative
  ( implement
  )

import qualified TSL.Writer.Arrow as Arrow
  ( implement
  )

import qualified TSL.Writer.Monadic as Monadic
  ( implement
  )

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

implement
  :: CodeTarget -> ModuleName -> FunctionName -> CFM -> String

implement = \case
  Applicative -> Applicative.implement
  Arrow -> Arrow.implement
  Clash -> Clash.implement
  Monadic -> Monadic.implement

-----------------------------------------------------------------------------

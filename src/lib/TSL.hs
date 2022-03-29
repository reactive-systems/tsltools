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
  , splitAssumptions
  , specifications2dependencies
  , tlsfToTslTerm
    -- * CFM Utilities
  , CodeTarget(..)
  , CFM
  , ModuleName
  , FunctionName
  , fromCFM
  , statistics
  , symbolTable
  , implement
  , implementHoa
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

import TSL.TLSF 
  ( toTLSF
  , tlsfToTslTerm 
  )

import TSL.Splitter (split, splitAssumptions)

import TSL.Dependency
  ( DependencyRepresentation(..)
  , specifications2dependencies
  )

import TSL.Simulation (simulate)

import TSL.TOML (toTOML)

import TSL.CFM (CFM, fromCFM, statistics, symbolTable)

import qualified TSL.Writer.CFM.Clash as Clash (implement)
import qualified TSL.Writer.CFM.Applicative as Applicative (implement)
import qualified TSL.Writer.CFM.Arrow as Arrow (implement)
import qualified TSL.Writer.CFM.Monadic as Monadic (implement)
import qualified TSL.Writer.CFM.JavaScript as JavaScript (implement)
import qualified TSL.Writer.CFM.WebAudio as WebAudio (implement)

import qualified Hanoi as H (HOA(..))
import Data.Maybe ( fromJust, maybeToList )
import qualified TSL.Writer.HOA.Python as Python (implementHoa)
import qualified TSL.Writer.HOA.JavaScript as JS (implementHoa)
import qualified TSL.Writer.HOA.XState as XState (implementHoa)

-----------------------------------------------------------------------------

data CodeTarget
  = Applicative
  | Monadic
  | Arrow
  | Clash
  | JavaScript
  | WebAudio
  | Python
  | JS
  | XState
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------

type ModuleName = String
type FunctionName = String

-----------------------------------------------------------------------------


implementHoa
  :: CodeTarget -> H.HOA -> String
implementHoa = \case
  Python      -> Python.implementHoa
  XState      -> XState.implementHoa
  JS  	      -> JS.implementHoa
  _           -> error "Unsupported language target for given format"

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
  _           -> error "Unsupported language target for given format"

-----------------------------------------------------------------------------

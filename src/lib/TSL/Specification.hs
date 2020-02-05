-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Specification
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Internal data structure of a specification.
--
------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Specification
  ( Specification(..)
  , TSLSpecification(..)
  , TSLStringSpecification(..)
  , tslSpecToTSLStrSpec
  , tslSpecToSpec
  ) where

-----------------------------------------------------------------------------
import TSL.Logic (Formula(..))

import TSL.SymbolTable (SymbolTable, stName)

-----------------------------------------------------------------------------
data Specification =
  Specification
    { -- | TSL formula
      formula :: Formula Int
      -- | symbol table containing information about identifiers
    , symboltable :: SymbolTable
    }

-----------------------------------------------------------------------------
data TSLSpecification =
  TSLSpecification
    { -- | List of TSL formulas that are assumed
      assumptions :: [Formula Int]
      -- | List of TSL formulas that should be guaranteed
    , guarantees :: [Formula Int]
      -- | symbol table containing information about identifiers
    , tslSymboltable :: SymbolTable
    }

---------------------------------------------------------
data TSLStringSpecification =
  TSLStringSpecification
    { -- | List of TSL formulas that are assumed
      assumptionsStr :: [Formula String]
      -- | List of TSL formulas that should be guaranteed
    , guaranteesStr :: [Formula String]
    }

-----------------------------------------------------------------------------
tslSpecToSpec :: TSLSpecification -> Specification
tslSpecToSpec TSLSpecification {..} =
  Specification
    { formula = Implies (And assumptions) (And guarantees)
    , symboltable = tslSymboltable
    }

-----------------------------------------------------------------------------
tslSpecToTSLStrSpec :: TSLSpecification -> TSLStringSpecification
tslSpecToTSLStrSpec TSLSpecification {..} =
  TSLStringSpecification
    { assumptionsStr = fmap (fmap (stName tslSymboltable)) assumptions
    , guaranteesStr = fmap (fmap (stName tslSymboltable)) guarantees
    }

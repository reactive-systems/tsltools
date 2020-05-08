-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Specification
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Internal data structure of a specification.
--
------------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Specification
  ( Specification(..)
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
  )

import TSL.SymbolTable
  ( SymbolTable
  )

-----------------------------------------------------------------------------

data Specification =
  Specification
    { -- | TSL formula
      formula :: Formula Int
    , -- | List of TSL formulas that are assumed
      assumptions :: [Formula Int]
    , -- | List of TSL formulas that should be guaranteed
      guarantees :: [Formula Int]
    , -- | symbol table containing information about identifiers
      symboltable :: SymbolTable
    }

-----------------------------------------------------------------------------

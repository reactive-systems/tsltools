-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Specification
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Internal data structure of a specification.
--
-----------------------------------------------------------------------------

module TSL.Specification
  ( Specification(..)
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula
  )

import TSL.SymbolTable
  ( SymbolTable
  )

-----------------------------------------------------------------------------

data Specification =
  Specification
    { -- | TSL formula
      formula :: Formula Int
    , -- | symbol table containing information about identifiers
      symboltable :: SymbolTable
    }

-----------------------------------------------------------------------------

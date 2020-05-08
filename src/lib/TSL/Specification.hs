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
  , toTSL
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( Formula(..)
  , tslFormula
  )

import TSL.SymbolTable
  ( SymbolTable
  , stName
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

-- | Prints a TSL specification in the TSL format

toTSL
  :: Specification -> String

toTSL Specification{..} =
  "assume {" ++ prFormulas assumptions ++ "\n}\n\n" ++
  "guarantee {" ++ prFormulas guarantees ++ "\n}"

  where
    prFormulas =
      concatMap $ ("\n  " ++) . (++ ";") . tslFormula (stName symboltable)

-----------------------------------------------------------------------------

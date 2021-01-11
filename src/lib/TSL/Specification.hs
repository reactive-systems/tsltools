-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Specification
-- Maintainer  :  Felix Klein
--
-- Internal data structure of a specification.
--
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module TSL.Specification
  ( Specification(..)
  , toFormula
  , toTSL
  ) where

-----------------------------------------------------------------------------

import TSL.Logic (Formula(..), tslFormula)

import TSL.SymbolTable (SymbolTable, stName)

-----------------------------------------------------------------------------

data Specification =
  Specification
    { -- | List of TSL formulas that are assumed
      assumptions :: [Formula Int]
    , -- | List of TSL formulas that should be guaranteed
      guarantees :: [Formula Int]
    , -- | symbol table containing information about identifiers
      symboltable :: SymbolTable
    }

-----------------------------------------------------------------------------

-- | Create one formula out of assumptions and guarantees.

toFormula
  :: [Formula Int] -> [Formula Int] -> Formula Int

toFormula assumptions guarantees =
  case (assumptions, guarantees) of
    (_,[])    -> TTrue
    ([],[g])  -> g
    ([],gs)   -> And gs
    ([a],[g]) -> Implies a g
    ([a],gs)  -> Implies a $ And gs
    (as,[g])  -> Implies (And as) g
    (as,gs)   -> Implies (And as) $ And gs

-----------------------------------------------------------------------------

-- | Prints a TSL specification in the TSL format

toTSL
  :: Specification -> String

toTSL Specification{..} =
  "assume {" ++ prFormulas assumptions ++ "\n}\n\n" ++
  "guarantee {" ++ prFormulas guarantees ++ "\n}\n"

  where
    prFormulas =
      concatMap $ ("\n  " ++) . (++ ";") . tslFormula (stName symboltable)

-----------------------------------------------------------------------------

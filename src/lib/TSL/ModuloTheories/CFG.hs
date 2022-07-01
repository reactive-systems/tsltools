-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.CFG
-- Description :  Builds a CFG for cells and outputs from the specification.
-- Maintainer  :  Wonhyuk Choi
--
-- This module builds a Context-Free Grammar for cell and output signals
-- from the original specification.
-- This is necessary to build a Syntax-Guided Synthesis grammar when
-- transforming a TSL-MT specification to TSL.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.CFG
  ( CFG(..)
  , fromSpec
  ) where

-------------------------------------------------------------------------------

import Data.Array(Array, array, assocs, bounds, indices, (//), (!))

import TSL.Logic ( Formula(..)
                 , SignalTerm(..)
                 , foldFormula
                 )

import TSL.Specification (Specification(..))

import TSL.SymbolTable (Id, SymbolTable(..), Kind(..))

-------------------------------------------------------------------------------

type Grammar = Array Id [SignalTerm Id]

data CFG = CFG
    { -- | CFG implemented as an array of lists.
      -- To get the possible production rules for each,
      -- index into the grammar with the appropriate signal Id.
        grammar  :: Grammar
    ,   symTable :: SymbolTable
    }

instance Show CFG where
    show (CFG g s) = 
        unlines $ map show' $ filter (\(fst, _) -> isUpdate fst) $ assocs g
      where
        show' (id, rules)  = unhashId id ++ " :\n" ++ show'' rules
        show''             = unlines . (map (('\t':) . show . (fmap unhashId)))
        isUpdate idx       = (stKind s) idx == Output
        unhashId           = stName s

fromSpec :: Specification -> CFG
fromSpec (Specification a g s) =
    CFG {
            grammar     = buildGrammar (a ++ g) grammarInit
        ,   symTable    = s
        }
  where
    symTableArr = symtable s
    grammarInit = array (bounds symTableArr) emptyRules
    emptyRules  = [(idx, []) | idx <- indices symTableArr]

buildGrammar :: [Formula Id] -> Grammar -> Grammar
buildGrammar [] g     = g
buildGrammar (x:xs) g = buildGrammar xs (foldFormula extendGrammar g x)

-- | Adds new production rules to the grammar.
extendGrammar :: Formula Id -> Grammar -> Grammar
extendGrammar (Update dst src) oldGrammar = newGrammar
  where
    oldRules   = oldGrammar ! dst
    newRules   = src:oldRules
    newGrammar = oldGrammar // [(dst, newRules)]
extendGrammar _ g = g

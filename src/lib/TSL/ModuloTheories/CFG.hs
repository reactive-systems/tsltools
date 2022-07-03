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

import TSL.Types (arity)

import TSL.Logic ( Formula(..)
                 , SignalTerm(..)
                 , foldFormula
                 )

import TSL.Specification (Specification(..))

import TSL.SymbolTable (Id, SymbolTable(..), Kind(..))

import TSL.ModuloTheories.AST(AST, fromSignalTerm)

-------------------------------------------------------------------------------

data CFG = CFG
    { -- | CFG implemented as an array of lists.
      -- To get the possible production rules for each,
      -- index into the grammar with the appropriate signal Id.
        grammar  :: Array Id [AST Id]
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
            grammar     = fmap (map fromSTerm) grammar'
        ,   symTable    = s
        }
  where
    symTableArr = symtable s
    fromSTerm   = fromSignalTerm (arity . (stType s))
    grammar'    = buildGrammar (a ++ g) grammarInit
    grammarInit = array (bounds symTableArr) emptyRules
    emptyRules  = [(idx, []) | idx <- indices symTableArr]

buildGrammar
    :: [Formula Id]
    -> Array Id [SignalTerm Id]
    -> Array Id [SignalTerm Id]
buildGrammar [] g     = g
buildGrammar (x:xs) g = buildGrammar xs (foldFormula extendGrammar g x)

-- | Adds new production rules to the grammar.
extendGrammar
    :: Formula Id
    -> Array Id [SignalTerm Id]
    -> Array Id [SignalTerm Id]
extendGrammar (Update dst src) oldGrammar = newGrammar
  where
    oldRules   = oldGrammar ! dst
    newRules   = src:oldRules
    newGrammar = oldGrammar // [(dst, newRules)]
extendGrammar _ g = g

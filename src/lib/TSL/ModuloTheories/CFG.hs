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

import TSL.Logic (Formula(..)
                 , SignalTerm(..)
                 , FunctionTerm(..)
                 , PredicateTerm(..)
                 )

import TSL.Specification (Specification(..))

import TSL.SymbolTable (SymbolTable(..), Kind(..))

import Data.Array(Array, array, assocs, bounds, indices, (//), (!))

-------------------------------------------------------------------------------

type Id = Int
type Grammar = Array Id [SignalTerm Id]

data CFG = CFG
    { -- | CFG implemented as an array of lists.
      -- To get the possible production rules for each,
      -- index into the grammar with the appropriate signal Id.
        grammar :: Grammar
    ,
        symTable :: SymbolTable
    }

instance Show CFG where
    show (CFG g s) = 
        unlines $ map show' $ filter (\(fst, _) -> isUpdate fst) $ assocs g
      where
        show' (id, rules)  = show (unhashId id) ++ " :\n" ++ show'' rules
        show''             = unlines . (map (('\t':) . show . unhashS))
        isUpdate idx       = (stKind s) idx == Output

        unhashId                    = (stName s) 

        unhashS (Signal s)          = unhashId s
        unhashS (FunctionTerm f)    = unhashF f
        unhashS (PredicateTerm p)   = unhashP p

        unhashF (FunctionSymbol f)  = unhashId f
        unhashF (FApplied f s)      = unhashF f ++ " " ++ unhashS s

        unhashP BooleanTrue         = "True"
        unhashP BooleanFalse        = "False"
        unhashP (BooleanInput b)    = unhashId b
        unhashP (PredicateSymbol p) = unhashId p
        unhashP (PApplied p s)      = unhashP p ++ " " ++ unhashS s

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
buildGrammar (x:xs) g = buildGrammar xs (extendGrammar x g)

-- | Adds new production rules to the grammar.
extendGrammar :: Formula Id -> Grammar -> Grammar
extendGrammar (Update dst src) oldGrammar = newGrammar
  where
    oldRules   = oldGrammar ! dst
    newRules   = src:oldRules
    newGrammar = oldGrammar // [(dst, newRules)]
extendGrammar (Not f) g          = extendGrammar f g
extendGrammar (Implies f h) g    = extendGrammar f (extendGrammar h g)
extendGrammar (Equiv f h) g      = extendGrammar f (extendGrammar h g)
extendGrammar (And (x:xs)) g     = extendGrammar (And xs) (extendGrammar x g)
extendGrammar (Or (x:xs))  g     = extendGrammar (Or xs) (extendGrammar x g)
extendGrammar (Next f) g         = extendGrammar f g
extendGrammar (Previous f) g     = extendGrammar f g
extendGrammar (Globally f) g     = extendGrammar f g
extendGrammar (Finally f) g      = extendGrammar f g
extendGrammar (Historically f) g = extendGrammar f g
extendGrammar (Once f) g         = extendGrammar f g
extendGrammar (Until f h) g      = extendGrammar f (extendGrammar h g)
extendGrammar (Release f h) g    = extendGrammar f (extendGrammar h g)
extendGrammar (Weak f h) g       = extendGrammar f (extendGrammar h g)
extendGrammar (Since f h) g      = extendGrammar f (extendGrammar h g)
extendGrammar (Triggered f h) g  = extendGrammar f (extendGrammar h g)
extendGrammar _ g                = g

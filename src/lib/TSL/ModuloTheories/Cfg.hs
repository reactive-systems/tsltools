-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Cfg
-- Description :  Builds a Cfg for cells and outputs from the specification.
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
module TSL.ModuloTheories.Cfg
  ( Cfg(..)
  , cfgFromSpec
  , outputSignals
  , productionRules
  ) where

-------------------------------------------------------------------------------

import qualified Data.Array as Array

import qualified Data.Map as Map

import Control.Applicative (liftA2)

import Data.Array (Array, (//), (!))

import Data.Map (Map)

import Data.Set (Set)

import TSL.Error (Error)

import TSL.Types (arity)

import TSL.Logic ( Formula(..)
                 , SignalTerm(..)
                 , foldFormula
                 , outputs
                 )

import TSL.Specification (Specification(..))

import TSL.SymbolTable (Id, SymbolTable(..), Kind(..))

import TSL.Ast (Ast, fromSignalTerm)

import TSL.ModuloTheories.Theories( TAst
                                  , TheorySymbol
                                  , Theory
                                  , read2Symbol
                                  , applySemantics
                                  )

-------------------------------------------------------------------------------

data Cfg = Cfg (Map TheorySymbol [TAst])

instance Show Cfg where
  show (Cfg cfg) = unlines $ map showPair $ Map.assocs cfg
    where
      showSymbol     = (++ " :\n") . show
      showRules      = unlines . (map (('\t':) . show))
      showPair (k,v) = showSymbol k ++ showRules v

cfgFromSpec :: Theory -> Specification -> Either Error Cfg
cfgFromSpec t s = cfgArr2Map t $ arrCfgFromSpec s

cfgArr2Map :: Theory -> ArrCfg -> Either Error Cfg
cfgArr2Map theory (ArrCfg grammar symTable) =
  (Cfg . Map.fromList) <$> newGrammar
  where
    unhash          = stName symTable
    toTAst          = (applySemantics theory) . (fmap unhash)
    toTSymbol       = read2Symbol theory . unhash
    theorize (k,vs) = liftA2 (,) (toTSymbol k) (traverse toTAst vs)
    newGrammar      = traverse theorize $ Array.assocs grammar

outputSignals :: Cfg -> Set TheorySymbol
outputSignals (Cfg grammar) = Map.keysSet grammar

productionRules :: TheorySymbol -> Cfg -> [TAst]
productionRules ts (Cfg grammar) = Map.findWithDefault [] ts grammar 

data ArrCfg = ArrCfg
    { -- | ArrCfg implemented as an array of lists.
      -- To get the possible production rules for each,
      -- index into the grammar with the appropriate signal Id.
        grammar  :: Array Id [Ast Id]
    ,   symTable :: SymbolTable
    }

arrCfgFromSpec :: Specification -> ArrCfg
arrCfgFromSpec (Specification a g s) =
    ArrCfg {
            grammar     = fmap (map fromSTerm) grammar'
        ,   symTable    = s
        }
  where
    symTableArr = symtable s
    fromSTerm   = fromSignalTerm (arity . (stType s))
    grammar'    = buildGrammar (a ++ g) grammarInit
    grammarInit = Array.array (Array.bounds symTableArr) emptyRules
    emptyRules  = [(idx, []) | idx <- Array.indices symTableArr]

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

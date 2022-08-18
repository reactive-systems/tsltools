-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Generates SyGuS problems from a Data Transformation Obligation.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus
  ( 
  ) where

-------------------------------------------------------------------------------
import TSL.Ast(AstInfo(..), SymbolInfo(..))

import TSL.ModuloTheories.CFG(CFG(..))

import TSL.ModuloTheories.Predicates(TheoryPredicate, predInfo)

import TSL.ModuloTheories.Theories( Theory
                                  , TheorySymbol
                                  , symbol2Smt
                                  , symbolType
                                  , smtSortDecl
                                  , isUninterpreted
                                  )

-------------------------------------------------------------------------------
-- | Data Transformation Obligation.
data DTO a = DTO 
    { -- | 
        preCondition  :: TheoryPredicate
    ,   postCondition :: TheoryPredicate
    }

preCondition2Sygus :: TheoryPredicate -> String
preCondition2Sygus = undefined

postCondition2Sygus :: TheoryPredicate -> String
postCondition2Sygus = undefined

-- TODO
-- | Gets all signals that SyGuS may need to update
-- to obtain a realizable underapproximation to TSL.
-- Intuitively, these are the cell & output signals in the post-condition.
-- Currently, it is approximated as all the signals in post-condition.
getSygusTargets :: DTO -> [TheorySymbol]
getSygusTargets (DTO _ post) = map symbol $ varInfos $ predInfo post

-- TODO
-- | Builds a SyGuS query from a
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusQuery :: DTO -> CFG -> String
sygusQuery dto@(DTO pre post) (CFG g _) = undefined
  where
    sygusTargets = getSygusTargets dto

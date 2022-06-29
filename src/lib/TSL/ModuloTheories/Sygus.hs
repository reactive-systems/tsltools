-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.CVC5
-- Description :  Utilities to send SMT and SyGuS problems to CVC5.
-- Maintainer  :  Wonhyuk Choi
--
-- Used for sending SMT and SyGuS problems to CVC5.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-
   TODO:
   * Send SMT/SyGuS problems to CVC5
   * Parse results from CVC5
   * Transform CVC5 results into some internal function representation
-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.CVC5
  ( 
  ) where

-------------------------------------------------------------------------------
import TSL.ModuloTheories.CFG(CFG(..))

import TSL.Logic(PredicateTerm(..))

import TSL.SymbolTable(Id)

-------------------------------------------------------------------------------
-- | Data Transformation Obligation.
data DTO a = DTO 
    { -- | 
        preCondition  :: PredicateTerm a
    ,   postCondition :: PredicateTerm a
    }

-- TODO
-- | Gets all signals that SyGuS may need to update
-- to obtain a realizable underapproximation to TSL.
-- Intuitively, these are the cell & output signals in the post-condition.
getSygusTargets :: DTO a -> [a]
getSygusTargets (DTO _ post) = undefined

-- TODO
-- | Builds a SyGuS query from a
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusQuery :: DTO -> CFG -> String
sygusQuery dto@(DTO pre post) (CFG g _) = undefined
  where
    sygusTargets = getSygusTargets dto

-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.ConsistencyChecking
-- Description :  
-- Maintainer  :  Wonhyuk Choi
--

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.ConsistencyChecking(consistencyChecking) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories(Theory)

import TSL.ModuloTheories.PredicateList(PredicateLiteral, enumeratePreds)

import qualified TSL.ModuloTheories.PredicateList as PLit(toSMT2)

-------------------------------------------------------------------------------
    
{-
   1. convert function symbols to appropriate symbol
-}

consistencyChecking
    :: (Show a)
    => Theory
    -> (String -> Bool)
    -> [PredicateLiteral a]
    -> [String]
consistencyChecking theory smtSolver =
  (map toTslAssumption) . (filter notSat) . enumeratePreds
    where notSat = not . smtSolver . (checkSatSmt2 theory)

checkSatSmt2 :: Show a => Theory -> PredicateLiteral a -> String
checkSatSmt2 theory p = unlines $ logic:variables:assert:checkSAT:[]
  where
    logic     = "(set-logic " ++ show theory ++ ")"
    variables = show p
    assert    = show p
    checkSAT  = "(check-sat)"

toTslAssumption :: Show a => PredicateLiteral a -> String
toTslAssumption p = show p ++ ";"

-- (set-logic LIA)
-- (declare-const vruntime2 Int)
-- (declare-const vruntime1 Int)

-- (assert (and (not (> vruntime2 vruntime1)) (not (> vruntime2 vruntime1))))
-- (check-sat)

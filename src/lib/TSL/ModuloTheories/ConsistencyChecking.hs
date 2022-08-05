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

import Control.Monad(filterM)

import TSL.Error(Error)

import TSL.Ast(stringifyAst)

import TSL.ModuloTheories.Theories( Theory )

import TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , enumeratePreds
                                       -- , getPLitVars
                                       )

import TSL.ModuloTheories.Solver(checkSat)

-------------------------------------------------------------------------------

consistencyChecking = undefined

-- consistencyChecking
--     :: Theory
--     -> [PredicateLiteral TheorySymbol]
--     -> Either Error [String]
-- consistencyChecking theory plist = 
--   (map toTslAssumption) <$> (filterM checkNotSat predicates)
--   where  
--     predicates        = enumeratePreds plist
--     checkNotSat       = (fmap not) . checkSat . (checkSatPred theory)
--     toTslAssumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

-- checkSatPred :: Theory -> PredicateLiteral TheorySymbol -> String
-- checkSatPred theory p = unlines $ [logic, variables, assert, checkSAT]
--   where
--     logic       = "(set-logic " ++ show theory ++ ")"
--     variables   = unlines $ map declConst $ getPLitVars p
--     assert      = "(assert " ++ pred2Smt p ++ ")"
--     checkSAT    = "(check-sat)"
--     declConst x =
--       "(declare-const " ++ toSmt x ++ " " ++ symbolType x ++ ")"

-- -- (set-logic LIA)
-- -- (declare-const vruntime2 Int)
-- -- (declare-const vruntime1 Int)

-- -- (assert (and (not (> vruntime2 vruntime1)) (not (> vruntime2 vruntime1))))
-- -- (check-sat)

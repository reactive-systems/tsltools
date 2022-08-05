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

-- pred2Smt :: PredicateLiteral TheorySymbol -> String
-- pred2Smt = \case
--   PLiteral p  -> stringifyAst toSmt p
--   NotPLit p   -> "(not " ++ pred2Smt p ++ ")"
--   OrPLit p q  -> "(or "  ++ pred2Smt p ++ " " ++ pred2Smt q ++ ")"
--   AndPLit p q -> "(and " ++ pred2Smt p ++ " " ++ pred2Smt q ++ ")"

-- pred2Tsl :: PredicateLiteral TheorySymbol -> String
-- pred2Tsl = \case
--   PLiteral p  -> stringifyAst toTsl p
--   NotPLit p   -> "!" ++ pred2Tsl p
--   OrPLit p q  -> "(" ++ pred2Tsl p ++ " || " ++ pred2Tsl q ++ ")"
--   AndPLit p q -> "(" ++ pred2Tsl p ++ " && " ++ pred2Tsl q ++ ")"

-- -- (set-logic LIA)
-- -- (declare-const vruntime2 Int)
-- -- (declare-const vruntime1 Int)

-- -- (assert (and (not (> vruntime2 vruntime1)) (not (> vruntime2 vruntime1))))
-- -- (check-sat)

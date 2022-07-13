-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Solver
-- Description :  Utilities to send SMT and SyGuS problems to a solver
--                and parse their results.
--                The choice of solver is extensible,
--                but currently it is hardcoded as CVC5 for now.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Solver( SolverErr(..)
                                , checkSat) where

-------------------------------------------------------------------------------

import TSL.Ast(Ast)

import TSL.ModuloTheories.Theories(Theory, TheorySymbol)

-------------------------------------------------------------------------------

data SolverErr = SolverErr String deriving (Show)

-- TODO: I/O
solve :: String -> String -> Either SolverErr String
-- solve args problem = Right ""
solve _ _ = Right ""

isSat :: String -> Either SolverErr Bool
isSat "sat"   = Right True
isSat "unsat" = Right False
isSat err     = Left $ SolverErr err

checkSat :: String -> Either SolverErr Bool
checkSat problem = solve problem "" >>= isSat

-- TODO
getModel :: Theory -> String -> Either SolverErr (Maybe TheorySymbol)
getModel _ _ = Right Nothing

-- TODO
getFunction :: Theory -> String -> Either SolverErr (Maybe (Ast TheorySymbol))
getFunction _ _ = Right Nothing

sygus :: Theory -> Int -> String -> Either SolverErr (Maybe (Ast TheorySymbol))
-- sygus theory maxDepth problem = _
sygus _ _ _ = Right Nothing

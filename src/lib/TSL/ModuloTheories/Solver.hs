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
module TSL.ModuloTheories.Solver (solveSat) where

-------------------------------------------------------------------------------

import System.Process(readProcess)

import TSL.Error(Error, errSolver)

import TSL.Ast(Ast)

import TSL.ModuloTheories.Theories(Theory, TAst)

-------------------------------------------------------------------------------

isSat :: String -> Either Error Bool
isSat "sat"   = Right True
isSat "unsat" = Right False
isSat err     = errSolver err

solveSat :: FilePath -> String -> IO (Either Error Bool)
solveSat solverPath problem = do
  output <- readProcess solverPath smt2 problem
  return $ isSat output
  where smt2 = ["--lang=smt2"]

-- TODO
getModel :: Theory -> String -> Either Error (Maybe TAst)
getModel theory problem = undefined

-- TODO
parseFunction :: Theory -> String -> TAst
parseFunction theory fxnStr = undefined

sygus :: Theory -> Int -> String -> Either Error (Maybe TAst)
-- sygus theory maxDepth problem = _
sygus theory maxDepth problem = Right Nothing

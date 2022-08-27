-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Solver
-- Description :  Utilities to send SMT and SyGuS problems to a solver
--                and parse their results.
--                The choice of solver is extensible,
--                but currently it is hardcoded as CVC5 for now.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Solver (solveSat) where

-------------------------------------------------------------------------------

import qualified Data.Text as Text

import Control.Monad.Trans.Except

import Control.Monad(liftM)

import System.Process(readProcessWithExitCode)

import System.Exit(ExitCode(..), die)

import TSL.Error(Error, errSolver)

import TSL.ModuloTheories.Theories(Theory, TheorySymbol, TAst)

-------------------------------------------------------------------------------

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

isSat :: String -> Either Error Bool
isSat "sat"   = Right True
isSat "unsat" = Right False
isSat err     = errSolver err

solveSat :: FilePath -> String -> ExceptT Error IO Bool
solveSat solverPath problem = ExceptT satResult
  where
    smt2         = ["--lang=smt2"]
    solverResult = runSolver solverPath smt2 problem
    satResult    = liftM (isSat . strip) solverResult

runSolver :: FilePath -> [String] -> String -> IO String
runSolver solverPath args problem = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode solverPath args problem
  case exitCode of
    ExitSuccess      -> return stdout
    ExitFailure code ->
      die $ "Process Error " ++ show code ++ ":" ++ stderr ++ "\n" ++ stdout

-- -- TODO
-- getModel :: FilePath -> String -> ExceptT Error IO TheorySymbol
-- getModel solverPath problem = undefined
--   where model = "(set-option :produce-models true)"

-- -- TODO
-- parseFunction :: Theory -> String -> Either Error TAst
-- parseFunction theory fxnStr = undefined

-- -- TODO
-- sygus :: FilePath -> Int -> String -> ExceptT Error IO (Maybe TAst)
-- sygus solverPath maxDepth problem = undefined

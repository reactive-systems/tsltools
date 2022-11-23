-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  TSL.ModuloTheories.Solver
-- Description :  Utilities to send SMT and SyGuS problems to a solver
--                and parse their results.
--                The choice of solver is extensible,
--                but currently it is hardcoded as CVC5 for now.
-- Maintainer  :  Wonhyuk Choi
module TSL.ModuloTheories.Solver (solveSat, runGetModel, runSygusQuery) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import TSL.Error (Error, errSolver, errSygus)

-------------------------------------------------------------------------------

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

isSat :: String -> Either Error Bool
isSat "sat" = Right True
isSat "unsat" = Right False
isSat err = errSolver err

runSolver :: FilePath -> [String] -> String -> ExceptT Error IO String
runSolver solverPath args query =
  parseResult =<< (ExceptT $ fmap Right $ readProcessWithExitCode solverPath args query)
  where
    parseResult :: (ExitCode, String, String) -> ExceptT Error IO String
    parseResult (exitCode, stdout, stderr) = case exitCode of
      ExitSuccess -> return stdout
      ExitFailure code -> except $ errSolver errMsg
        where
          errMsg = show code ++ " >> " ++ stderr ++ "\n" ++ stdout

solveSat :: FilePath -> String -> ExceptT Error IO Bool
solveSat solverPath = ((=<<) toBoolean) . (runSolver solverPath smt2)
  where
    smt2 = ["--lang=smt2"]
    toBoolean = except . isSat . strip

runGetModel :: FilePath -> String -> ExceptT Error IO String
runGetModel solverPath = runSolver solverPath args
  where
    args = ["--lang=smt2"]

runSygusQuery :: FilePath -> Int -> String -> ExceptT Error IO String
runSygusQuery solverPath depth = ((=<<) getResult) . (runSolver solverPath args)
  where
    args = depthLimit : ["-o", "sygus-sol-gterm", "--lang=sygus2"]
    depthLimit = "--sygus-abort-size=" ++ show depth
    getResult result =
      except $
        if isInfixOf "error" result
          then errSygus result
          else Right result

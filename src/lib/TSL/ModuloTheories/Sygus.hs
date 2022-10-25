-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus (sygusTslAssumption) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error (Error, errSygus, parseError)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Solver (runSolver)

import TSL.ModuloTheories.Sygus.Common (Temporal(..), Dto)

import TSL.ModuloTheories.Sygus.Query (fixedSizeQuery)

import TSL.ModuloTheories.Sygus.Parser (parseSolution)

import TSL.ModuloTheories.Sygus.Assumption (sygus2TslAssumption)

-------------------------------------------------------------------------------

-- | A SyGuS Query is based off of:
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusTslAssumption
  :: FilePath
  -> (Temporal, Dto)
  -> Cfg
  -> ExceptT Error IO String
sygusTslAssumption solverPath problem cfg = queryResult >>= mkAssumption
  where queryResult  = runQuery solverPath problem cfg
        mkAssumption = ExceptT . return . (flip result2TslAssumption problem)

result2TslAssumption :: String -> (Temporal, Dto) -> Either Error String
result2TslAssumption result (temporal, dto) = 
  case parseSolution result of
    Left  err  -> parseError err
    Right term -> Right $ sygus2TslAssumption temporal dto term

runQuery
  :: FilePath
  -> (Temporal, Dto)
  -> Cfg
  -> ExceptT Error IO String
runQuery solverPath (temporal, dto) cfg =
  case sygusQuery of
    Nothing    -> except $ errSygus $ "No query for:\n" ++ show dto
    Just query -> ExceptT $ Right <$> (runSolver solverPath args query)
  where 
    args       = ["-o", "sygus-sol-gterm"] ++ depthLimit
    depthLimit = case temporal of
                   Next depth -> ["sygus-abort-size=" ++ show depth]
                   Eventually -> []
    sygusQuery = case temporal of
                   Next _     -> fixedSizeQuery dto cfg
                   Eventually -> undefined

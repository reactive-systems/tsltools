-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus ( generateAssumptions
                                , generateQueryAssumptionPairs
                                ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Control.Monad (liftM2)

import TSL.Error (Error, errSygus, parseError)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Solver (runSolver)

import TSL.ModuloTheories.Sygus.Common (Temporal(..), Dto)

import TSL.ModuloTheories.Sygus.Query (generateQuery)

import TSL.ModuloTheories.Sygus.Parser (parseSolution)

import TSL.ModuloTheories.Sygus.Assumption (sygus2TslAssumption)

-------------------------------------------------------------------------------

temporalAtoms :: [Temporal]
temporalAtoms = [Next 1]

generateAssumptions :: FilePath -> Cfg -> [Dto] -> IO String
generateAssumptions solverPath cfg dtos =
  (mkAlwaysAssumeBlock . unlines) <$> assumptions
  where
    problems    = (,) <$> temporalAtoms <*> dtos
    assumptions = traverse (extractAssumption . mkAssumption) problems

    mkAssumption (temporal, dto) = sygusTslAssumption solverPath cfg temporal dto
    mkAlwaysAssumeBlock str      = "always assume{\n" ++ str ++ "\n}\n"
    extractAssumption            = (fmap either2Assumption) . runExceptT
    either2Assumption            = \case
      Left  err                   -> "// " ++ show err
      Right assumption            -> assumption

-- | A SyGuS Query is based off of:
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusTslAssumption
  :: FilePath
  -> Cfg
  -> Temporal
  -> Dto
  -> ExceptT Error IO String
sygusTslAssumption solverPath cfg temporal dto = queryResult >>= mkAssumption
  where query        = generateQuery temporal cfg dto
        queryResult  = except query >>= (ExceptT . (fmap Right) . (runQuery solverPath temporal))
        mkAssumption = except . (result2TslAssumption temporal dto)

result2TslAssumption :: Temporal -> Dto -> String -> Either Error String
result2TslAssumption temporal dto result = 
  case parseSolution result of
    Left  err  -> parseError err
    Right term -> Right $ sygus2TslAssumption temporal dto term

runQuery :: FilePath -> Temporal -> String -> IO String
runQuery solverPath temporal = (fmap getGterm) . (runSolver solverPath args)
  where 
    getGterm   = head . lines
    args       = ["-o", "sygus-sol-gterm", "--lang=sygus2"] ++ depthLimit
    depthLimit = case temporal of
                   Next depth -> ["--sygus-abort-size=" ++ show depth]
                   Eventually -> []

generateQueryAssumptionPairs :: FilePath -> Cfg -> [Dto] -> [ExceptT Error IO (String, String)]
generateQueryAssumptionPairs solverPath cfg dtos = zipWith mkPair queries assumptions
  where
    problems    = (,) <$> temporalAtoms <*> dtos
    queries     = map (\(temporal, dto) -> generateQuery temporal cfg dto) problems
    results     = map mkResult $ zip queries $ map fst problems
    assumptions = map mkAssumption $ zipWith (\(a,b) c -> (a,b,c)) problems results

    mkResult (q,t) = (except q) >>= (ExceptT . (fmap Right) . (runQuery solverPath t))
    mkAssumption (t, dto, result) = result >>= (except . result2TslAssumption t dto)
    mkPair query assumption       = liftM2 (,) (except query) assumption

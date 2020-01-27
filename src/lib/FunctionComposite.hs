-----------------------------------------------------------------------------
-- |
-- Module      :  FunctionComposite.hs
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Plugging all tools together
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module FunctionComposite
  ( specAnalysis
  , lazySpecAnalysis
  ) where

-----------------------------------------------------------------------------
import CoreGen (Core(..), generateCore)
import Data.Maybe
import External.Context (Context, tslSpecCounterSynt, tslSpecSynt)
import Simulator.Simulator (Simulation, createSimulation)
import TSL (split)
import TSL.Aiger (Circuit)
import TSL.Specification (TSLSpecification)

-----------------------------------------------------------------------------
--
--  This are dummies, TODO replace by import
--
merge :: [Circuit] -> Circuit
merge _ = error "TODO Implement"

-----------------------------------------------------------------------------
--
-- Some basic notions
--
type Strategy = Circuit

type CounterStrategy = Circuit

type UnrealizableWitness = (Core, CounterStrategy, Simulation)

-----------------------------------------------------------------------------
--
-- Analyzes a specification, so either give a system strategy or some
-- cores with counter strategy and simulation
--
-- As some of the operations might be really costly it might be useful
-- to use parallelization
--
specAnalysis ::
     Context -> TSLSpecification -> IO (Either Strategy [UnrealizableWitness])
specAnalysis context spec = do
  let specs = split spec
  results <- sequence $ map (tslSpecSynt context) specs --Could be parallelized
  let syntResults = zip specs results
  if all (isJust . snd) syntResults
    then return $ Left $ merge (map (fromJust . snd) syntResults)
    else do
      let unrealSpec = map fst $ filter (isNothing . snd) syntResults
      cores <- sequence $ map (generateCore context) unrealSpec --Could be parallelized
      -- Some sanitizing BEGIN
      if all
           (\case
              NaC -> False
              _ -> True)
           cores
        then return ()
        else error "Assertion: Every core should be a real core"
      -- END
      witnesses <- sequence $ map (coreToWitness context) cores --Could be parallelized
      return $ Right $ witnesses

-----------------------------------------------------------------------------
--
-- Analyzes a specification, so either give a system strategy or one
-- cores with counter strategy and simulation, i.e. this is an incremental approach
--
lazySpecAnalysis ::
     Context -> TSLSpecification -> IO (Either Strategy UnrealizableWitness)
lazySpecAnalysis context spec = do
  let specs = split spec
  synt <- incrementalSynthesis context specs
  case synt of
    Left strats -> return $ Left $ merge $ strats
    Right spec -> do
      core <- generateCore context spec
      wit <- coreToWitness context core
      return $ Right $ wit

incrementalSynthesis ::
     Context -> [TSLSpecification] -> IO (Either [Strategy] TSLSpecification)
incrementalSynthesis _ [] = return $ Left []
incrementalSynthesis context (x:xr) = do
  syn <- tslSpecSynt context x
  case syn of
    Nothing -> return $ Right x
    Just s -> do
      rec <- incrementalSynthesis context xr
      case rec of
        Left sr -> return $ Left $ s : sr
        counterEx -> return $ counterEx

-----------------------------------------------------------------------------
--
-- Creates an unrealizabilty wittness out of a core
--
coreToWitness :: Context -> Core -> IO UnrealizableWitness
coreToWitness context core =
  case core of
    NaC -> undefined
    Unsat spec -> do
      res <- tslSpecCounterSynt context spec
      case res of
        Nothing -> error "Assertion: A core should yield a counter startegy"
        Just cstrat -> return (core, cstrat, createSimulation cstrat spec)
    Unrez spec -> do
      res <- tslSpecCounterSynt context spec
      case res of
        Nothing -> error "Assertion: A core should yield a counter startegy"
        Just cstrat -> return (core, cstrat, createSimulation cstrat spec)

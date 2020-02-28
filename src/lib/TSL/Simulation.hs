-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation
  ( SystemSimulation
  , SystemOption
  , createSimulation
  , step
  , rewind
  , options
  , getLog
  , simulate
  ) where

-----------------------------------------------------------------------------
import TSL.Reader (fromTSLtoTSLSpec)

import TSL.Error (genericError)

import TSL.Simulation.SystemSimulationBackend
  ( SystemOption
  , SystemSimulation(..)
  , getLog
  , options
  , rewind
  , sanitize
  , step
  )

import TSL.Simulation.SystemSimulationInterface (runSimulation)

import TSL.Aiger (Circuit, parseAag)

import TSL.Simulation.AigerSimulator (normalize)

import TSL.Logic (readInput, readOutput)

import TSL.Error (Error)

import TSL.Specification (TSLSpecification, tslSpecToTSLStrSpec)

import TSL.Simulation.FiniteTraceChecker as FTC (emptyTrace)

-----------------------------------------------------------------------------
-- | Generates a simulation out of a given an AIGER counterstrategy
-- and a correspoding TSL specification.
createSimulation :: Circuit -> TSLSpecification -> Either Error SystemSimulation
createSimulation aag spec =
  case normalize readOutput readInput aag of
    Left err -> Left err
    Right naag ->
      let sim =
            SystemSimulation
              { counterStrategy = naag
              , specification = tslSpecToTSLStrSpec spec
              , stateStack = [\_ -> False]
              , trace = emptyTrace
              , logTrace = []
              }
       in case sanitize sim of
            Nothing -> Right sim
            Just err -> genericError $ err

---------------------------------------------------------------------------
-- | Given the filepaths of the tsl file and the agg file, load and
-- run the simulation
simulate :: String -> String -> Either Error (IO ())
simulate tsl' aag' = do
  aag <- parseAag aag'
  tsl <- fromTSLtoTSLSpec tsl'
  case createSimulation aag tsl of
    Left err -> Left $ err
    Right sim -> Right $ runSimulation sim

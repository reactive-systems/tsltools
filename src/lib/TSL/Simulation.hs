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

import TSL.Simulation.SystemSimulationBackend as SysSim
  ( SystemOption
  , SystemSimulation(..)
  , getLog
  , options
  , rewind
  , sanitize
  , step
  )

import TSL.Simulation.EnvironmentSimulationBackend as EnvSim
  ( EnvironmentSimulation(..)
  , sanitize
  )

import TSL.Simulation.SystemSimulationInterface as SysSymInt (runSimulation)

import TSL.Simulation.EnvironmentSimulationInterface as EnvSymInt
  ( runSimulation
  )

import TSL.Aiger (Circuit, parseAag)

import TSL.Simulation.AigerSimulator (normalize)

import TSL.Logic (readInput, readOutput)

import TSL.Error (Error)

import TSL.Specification
  ( TSLSpecification
  , TSLStringSpecification(..)
  , tslSpecToTSLStrSpec
  )

import TSL.Simulation.FiniteTraceChecker as FTC (emptyTrace)

-----------------------------------------------------------------------------
-- | Generates a simulation out of a given an AIGER counterstrategy
-- and a correspoding TSL specification.
createSimulation ::
     Circuit
  -> TSLSpecification
  -> Either Error (Either SystemSimulation EnvironmentSimulation)
createSimulation aag spec =
  case normalize readOutput readInput aag of
    Right naag ->
      let stringSpec = tslSpecToTSLStrSpec spec
          sim =
            SystemSimulation
              { counterStrategy = naag
              , specification = stringSpec
              , stateStack = [\_ -> False]
              , trace =
                  emptyTrace
                    (assumptionsStr stringSpec, guaranteesStr stringSpec)
              , logTrace = []
              }
       in case SysSim.sanitize sim of
            Nothing -> Right (Left sim)
            Just err -> genericError $ err
    Left _ ->
      case normalize readInput readOutput aag of
        Right naag ->
          let stringSpec = tslSpecToTSLStrSpec spec
              sim =
                EnvironmentSimulation
                  { strategy = naag
                  , specification = stringSpec
                  , stateStack = [\_ -> False]
                  , trace = emptyTrace ([], assumptionsStr stringSpec)
                  , logTrace = []
                  }
           in case EnvSim.sanitize sim of
                Nothing -> Right (Right sim)
                Just err -> genericError $ err
        Left err -> Left err

---------------------------------------------------------------------------
-- | Given the filepaths of the tsl file and the agg file, load and
-- run the simulation
simulate :: String -> String -> Either Error (IO ())
simulate tsl' aag' = do
  aag <- parseAag aag'
  tsl <- fromTSLtoTSLSpec tsl'
  case createSimulation aag tsl of
    Left err -> Left $ err
    Right (Left sim) -> Right $ SysSymInt.runSimulation sim
    Right (Right sim) -> Right $ EnvSymInt.runSimulation sim

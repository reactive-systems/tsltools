-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulator
-- Maintainer  :  Philippe Heim
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards
  , ImplicitParams

  #-}

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

import TSL.Reader
  ( fromTSL
  )

import TSL.SymbolTable
  ( stName
  )

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

import TSL.Simulation.SystemSimulationInterface as SysSymInt
  ( runSimulation
  )

import TSL.Simulation.EnvironmentSimulationInterface as EnvSymInt
  ( runSimulation
  )

import TSL.Aiger
  ( Circuit
  , parseAag
  )

import TSL.Simulation.AigerSimulator
  ( normalize
  )

import TSL.Logic
  ( decodeInputAP
  , decodeOutputAP
  )

import TSL.Error
  ( Error
  , genericError
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.Simulation.FiniteTraceChecker as FTC
  ( emptyTrace
  )

-----------------------------------------------------------------------------

-- | Generates a simulation out of a given an AIGER counterstrategy
-- and a correspoding TSL specification.

createSimulation
  :: Circuit
  -> Specification
  -> Either Error (Either SystemSimulation EnvironmentSimulation)

createSimulation aag spec =
  case normalize decodeOutputAP decodeInputAP aag of
    Right naag ->
      let
        sim =
          SystemSimulation
            { counterStrategy = naag
            , specification = spec
            , stateStack = [const False]
            , trace =
                emptyTrace
                  (assumptionsStr spec, guaranteesStr spec)
            , logTrace = []
            }
       in
         case SysSim.sanitize sim of
           Nothing -> Right (Left sim)
           Just err -> genericError err
    Left _ ->
      case normalize decodeInputAP decodeOutputAP aag of
        Left err   -> Left err
        Right naag ->
          let
            sim =
              EnvironmentSimulation
                { strategy = naag
                , specification = spec
                , stateStack = [const False]
                , trace = emptyTrace ([], assumptionsStr spec)
                , logTrace = []
                }
           in
             case EnvSim.sanitize sim of
               Nothing -> Right (Right sim)
               Just err -> genericError err

  where
    assumptionsStr = fmap (fmap (stName $ symboltable spec)) . assumptions
    guaranteesStr = fmap (fmap (stName $ symboltable spec)) . guarantees

---------------------------------------------------------------------------

-- | Given the filepaths of the tsl file and the agg file, load and
-- run the simulation

simulate
  :: (?specFilePath :: Maybe FilePath) => String -> String -> IO (Either Error (IO ()))

simulate tsl' aag' =
  fromTSL tsl' >>= return . \case
    Left err  -> Left err
    Right tsl -> case  parseAag aag' of
      Left err  -> Left err
      Right aag -> case createSimulation aag tsl of
        Left err          -> Left err
        Right (Left sim)  -> Right $ SysSymInt.runSimulation sim
        Right (Right sim) -> Right $ EnvSymInt.runSimulation sim

---------------------------------------------------------------------------

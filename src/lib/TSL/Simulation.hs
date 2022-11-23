-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  TSL.Simulator
-- Description :  A (counter)strategy simulator
-- Maintainer  :  Philippe Heim
--
-- This module a simulation backend and TUI-frontend for simulating winning TSL
-- synthesis strategies and counter strategies. Given a specification and a
-- environment-strategy (system-strategy) the user can play the as the
-- system (environment) against the (counter)strategy by choosing outputs. The
-- simulator evaluates the played trace and tries to detect violations of the
-- specification and computes the open obligations. This allows the user to
-- see why some specification is unsatisfiable (satisfiable) by trying out why
-- a system in not implementable (what the system does on some inputs).
module TSL.Simulation
  ( SystemSimulation,
    SystemOption,
    createSimulation,
    step,
    rewind,
    options,
    getLog,
    simulate,
  )
where

-------------------------------------------------------------------------------

import TSL.Aiger (Circuit, parseAag)
import TSL.Error (Error, genericError)
import TSL.Logic (decodeInputAP, decodeOutputAP)
import TSL.Reader (fromTSL)
import TSL.Simulation.AigerSimulator (normalize)
import TSL.Simulation.EnvironmentSimulationBackend as EnvSim
  ( EnvironmentSimulation (..),
    sanitize,
  )
import TSL.Simulation.EnvironmentSimulationInterface as EnvSymInt
  ( runSimulation,
  )
import TSL.Simulation.FiniteTraceChecker as FTC (emptyTrace)
import TSL.Simulation.SystemSimulationBackend as SysSim
  ( SystemOption,
    SystemSimulation (..),
    getLog,
    options,
    rewind,
    sanitize,
    step,
  )
import TSL.Simulation.SystemSimulationInterface as SysSymInt (runSimulation)
import TSL.Specification (Specification (..))
import TSL.SymbolTable (stName)

-------------------------------------------------------------------------------

-- | 'createSimulation' generates a  simulation of a AIGER circuit/strategy and
-- a corresponirng TSL specification. Note that if these do not match an error
-- will be returned.
createSimulation ::
  Circuit ->
  Specification ->
  Either Error (Either SystemSimulation EnvironmentSimulation)
createSimulation aag spec =
  case normalize decodeOutputAP decodeInputAP aag of
    Right naag ->
      let sim =
            SystemSimulation
              { counterStrategy = naag,
                specification = spec,
                stateStack = [const False],
                trace =
                  emptyTrace
                    (assumptionsStr spec, guaranteesStr spec),
                logTrace = []
              }
       in case SysSim.sanitize sim of
            Nothing -> Right (Left sim)
            Just err -> genericError err
    Left _ ->
      case normalize decodeInputAP decodeOutputAP aag of
        Left err -> Left err
        Right naag ->
          let sim =
                EnvironmentSimulation
                  { strategy = naag,
                    specification = spec,
                    stateStack = [const False],
                    trace = emptyTrace ([], assumptionsStr spec),
                    logTrace = []
                  }
           in case EnvSim.sanitize sim of
                Nothing -> Right (Right sim)
                Just err -> genericError err
  where
    assumptionsStr = fmap (fmap (stName $ symboltable spec)) . assumptions
    guaranteesStr = fmap (fmap (stName $ symboltable spec)) . guarantees

-------------------------------------------------------------------------------

-- | 'simulate' takes the filepaths of the TSL file and the AIGER file, load
-- them, and runs an interactive simulation as TUI.
simulate ::
  Maybe FilePath -> String -> String -> IO (Either Error (IO ()))
simulate specPath tsl' aag' =
  ( \case
      Left err -> Left err
      Right tsl -> case parseAag aag' of
        Left err -> Left err
        Right aag -> case createSimulation aag tsl of
          Left err -> Left err
          Right (Left sim) -> Right $ SysSymInt.runSimulation sim
          Right (Right sim) -> Right $ EnvSymInt.runSimulation sim
  )
    <$> fromTSL specPath tsl'

-------------------------------------------------------------------------------

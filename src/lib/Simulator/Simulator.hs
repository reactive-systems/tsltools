-- |
-- Module      :  Simulator.Simulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module Simulator.Simulator where

-----------------------------------------------------------------------------
import TSL.Aiger (Circuit)

import Simulator.Core
  ( NormCircuit(inputName, inputs, latches, outputName, outputs)
  , State
  , normalize
  , simStep
  )

import TSL.Logic (Formula, PredicateTerm, SignalTerm, readInput, readOutput)

import TSL.Error (Error)

import TSL.Specification
  ( TSLSpecification
  , TSLStringSpecification
  , tslSpecToTSLStrSpec
  )

import Simulator.FiniteTraceChecker as FTC
  ( FiniteTrace
  , (|=)
  , append
  , emptyTrace
  , rewind
  )

-----------------------------------------------------------------------------
type CounterStrategy
   = NormCircuit (String, SignalTerm String) (PredicateTerm String)

data Simulation =
  Simulation
    { counterStrategy :: CounterStrategy
    , specification :: TSLStringSpecification
    , stateStack :: [State]
    , trace :: FiniteTrace String
    }

type Option = [(String, SignalTerm String)]

type Witness = [Formula String]

-----------------------------------------------------------------------------
--
-- Given a aag counterstrategy and a correspoding tsl spec
-- generates a simulation out of this
--
-- TODOs:
-- - Check that counterstartegy and simulation match each other
-- - Implement proper error handling for ap parsing
--
createSimulation :: Circuit -> TSLSpecification -> Simulation
createSimulation aag spec =
  Simulation
    { counterStrategy = normalize (parseAP readOutput) (parseAP readInput) aag
    , specification = tslSpecToTSLStrSpec spec
    , stateStack = [\_ -> False]
    , trace = emptyTrace
    }
  where
    parseAP :: (String -> Either Error a) -> String -> a
    parseAP parse str =
      case parse str of
        Left err -> error $ "Simulator: While parsing ap found " ++ (show err)
        Right val -> val

-----------------------------------------------------------------------------
--
-- Gives all options of a simulation and a list of TSLFormulas (Witness)
-- that would be violated
--
options :: Simulation -> [(Option, Witness)]
options _ = undefined --TODO

-----------------------------------------------------------------------------
--
--  Given an possible action option, simulate one step and calculate the
--  predicate evaluations
--
step :: Simulation -> Option -> (Simulation, [(PredicateTerm String, Bool)])
step sim@Simulation {..} _ =
  (sim {stateStack = q : stateStack, trace = newTrace}, eval)
  where
    q = undefined --TODO
    newTrace = undefined --TODO
    eval = undefined --TODO

-----------------------------------------------------------------------------
--
--  Rewind steps the simulation one step back
--
rewind :: Simulation -> Simulation
rewind sim@Simulation {stateStack = stateStack, trace = trace} =
  sim
    { stateStack =
        case stateStack of
          [] ->
            error
              "Simulator: Assertion, there should always be the initial state"
          [init] -> [init]
          _:sr -> sr
    , trace = FTC.rewind trace
    }

-- |
-- Module      :  Simulator.Simulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module Simulator.Simulator
  ( Simulation
  , createSimulation
  , step
  , Simulator.Simulator.rewind
  , options
  ) where

-----------------------------------------------------------------------------
import Data.List as List (filter)
import Data.Set as Set (filter, fromList, map, powerSet, toList)
import TSL.Aiger (Circuit)

import Simulator.Core
  ( NormCircuit(inputName, inputs, outputName, outputs)
  , State
  , normalize
  , simStep
  )

import TSL.Logic
  ( Formula(..)
  , PredicateTerm
  , SignalTerm(Signal)
  , SignalTerm
  , readInput
  , readOutput
  )

import TSL.Error (Error)

import TSL.Specification
  ( TSLSpecification
  , TSLStringSpecification(..)
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
options sim@Simulation {counterStrategy = ct, specification = spec} =
  zip options witnesses
  where
    options = possibleOptions ct
    posFinTraces = fmap (\o -> trace $ fst $step sim o) options
    witnesses = fmap (violatedGuarantees spec) posFinTraces

possibleOptions :: CounterStrategy -> [Option]
possibleOptions cst = fmap extendUpdates filteredCombinations
  where
    allUpdates = [inputName cst i | i <- inputs cst]
    cells = toList $ fromList $ fmap fst allUpdates
    --
    allCombinations = Set.map toList $ powerSet $ fromList allUpdates
    filteredCombinations =
      toList $ Set.filter (unique . (fmap fst)) $ allCombinations
    --
    unique [] = True
    unique (c:cr) = (not (elem c cr)) && unique cr
    --
    extendUpdates updates =
      foldl
        (\upds c ->
           if all ((/= c) . fst) upds
             then (c, Signal c) : upds
             else upds)
        updates
        cells

violatedGuarantees :: TSLStringSpecification -> FiniteTrace String -> Witness
violatedGuarantees TSLStringSpecification { assumptionsStr = assmpt
                                          , guaranteesStr = gar
                                          } trace =
  List.filter (\f -> not (trace |= Implies (And assmpt) f)) gar

-----------------------------------------------------------------------------
--
--  Given an possible action option, simulate one step and calculate the
--  predicate evaluations
--  ASSUMPTION: The option should be complete, i.e. on a higher level
--  for every cell in the formula, the circuit can update on of these cells
--
step :: Simulation -> Option -> (Simulation, [(PredicateTerm String, Bool)])
step sim@Simulation {..} updates =
  (sim {stateStack = q : stateStack, trace = newTrace}, eval)
  where
    input = \i -> elem (inputName counterStrategy i) updates -- The input for the c-strat circuit
    --
    (q, output) = simStep counterStrategy (head stateStack) input -- The c-strat simulation step
    --
    eval =
      [(outputName counterStrategy o, output o) | o <- outputs counterStrategy] --The predicate evaluation generated out of the output
    --
    newTrace =
      append
        trace
        (\c -> findFirst "Simulator.step: Assumption violated" (== c) updates)
        (\p ->
           findFirst
             "Simulator.step: Ouput should contain all predicates"
             (== p)
             eval)
    --
    findFirst :: String -> (a -> Bool) -> [(a, b)] -> b
    findFirst msg _ [] = error msg
    findFirst msg p ((a, b):xr) =
      if p a
        then b
        else findFirst msg p xr

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

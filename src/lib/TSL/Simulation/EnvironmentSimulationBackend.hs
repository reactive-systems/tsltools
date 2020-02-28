-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulaton.EnvironmentSimulationBackend
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- The backend of the environment simulation when playing againts a strategy/
-- system
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------v
module TSL.Simulation.EnvironmentSimulationBackend
  ( SystemStrategy
  , EnvironmentOption
  , EnvironmentSimulation(..)
  , Witness
  , options
  , step
  , rewind
  , getLog
  , sanitize
  ) where

-----------------------------------------------------------------------------
import Control.Exception (assert)

import TSL.Specification (TSLStringSpecification(..))

import qualified Data.List as List (filter)

import Data.Set as Set
  ( Set
  , difference
  , fromList
  , isSubsetOf
  , member
  , powerSet
  , toList
  )

import TSL.Simulation.AigerSimulator
  ( NormCircuit
  , State
  , inputName
  , inputs
  , outputName
  , outputs
  , simStep
  )

import TSL.Simulation.FiniteTraceChecker as FTC (FiniteTrace, (|=), append)

import qualified TSL.Simulation.FiniteTraceChecker as FTC (rewind)

import TSL.Logic (Formula(..), PredicateTerm, SignalTerm(..))

import TSL.FormulaUtils (getOutputs, getPredicates)

import TSL.ToString (predicateTermToString)

-----------------------------------------------------------------------------
-- | TODO
type SystemStrategy
   = NormCircuit (PredicateTerm String) (String, SignalTerm String)

-----------------------------------------------------------------------------
-- | TODO
data EnvironmentSimulation =
  EnvironmentSimulation
    { strategy :: SystemStrategy
    , specification :: TSLStringSpecification
    , stateStack :: [State]
    , trace :: FiniteTrace String
    , logTrace :: [(EnvironmentOption, [(String, SignalTerm String)])]
    }

-----------------------------------------------------------------------------
-- | TODO
type EnvironmentOption
   = (Set (PredicateTerm String), PredicateTerm String -> Bool)

-----------------------------------------------------------------------------
-- | TODO
type Witness = [Formula String]

-----------------------------------------------------------------------------
--
-- Gives all options of a simulation and a list of TSLFormulas (Witness)
-- that would be violated
--
options ::
     EnvironmentSimulation
  -> [(EnvironmentOption, Witness, [(String, SignalTerm String)])]
options sim@EnvironmentSimulation {strategy = ct, specification = spec} =
  zip3 options witnesses evaluations
  where
    options = possibleOptions ct
    steps = fmap (step sim) options
    witnesses = fmap ((violatedAssumptions spec) . trace . fst) steps
    evaluations = fmap snd steps

possibleOptions :: SystemStrategy -> [EnvironmentOption]
possibleOptions cst =
  let allPredicates = [inputName cst i | i <- inputs cst]
      allPredicateChoices = toList $ powerSet $ fromList allPredicates
   in fmap
        (\s -> (fromList allPredicates, \p -> p `member` s))
        allPredicateChoices

violatedAssumptions :: TSLStringSpecification -> FiniteTrace String -> Witness
violatedAssumptions TSLStringSpecification {assumptionsStr = assmpt} trace =
  List.filter (\f -> not (trace |= f)) assmpt

-----------------------------------------------------------------------------
--  | Given an possible action option, simulate one step and calculate the
--  update choices
--  ASSUMPTION: The option should be complete, i.e. on a higher level
--  for every cell in the formula, the circuit can update on of these cells 
--  (can be checked using sanitize)
--
step ::
     EnvironmentSimulation
  -> EnvironmentOption
  -> (EnvironmentSimulation, [(String, SignalTerm String)])
step sim@EnvironmentSimulation {..} (predicates, predEval) =
  (sim {stateStack = q : stateStack, trace = newTrace, logTrace = newLog}, eval)
  where
    (q, output) =
      simStep strategy (head stateStack) (predEval . (inputName strategy))
    --
    eval = [outputName strategy o | o <- outputs strategy, output o]
    --
    newTrace = append trace (\c -> findFirst (== c) eval) predEval
    --
    newLog = ((predicates, predEval), eval) : logTrace
    --
    findFirst :: (a -> Bool) -> [(a, b)] -> b
    findFirst _ [] = assert False undefined --THIS cant happend iff the simulation is sanitized
    findFirst p ((a, b):xr) =
      if p a
        then b
        else findFirst p xr

-----------------------------------------------------------------------------
-- | Rewind steps the simulation one step back
rewind :: EnvironmentSimulation -> EnvironmentSimulation
rewind sim@EnvironmentSimulation { stateStack = stateStack
                                 , trace = trace
                                 , logTrace = logTrace
                                 } =
  sim
    { stateStack =
        case stateStack of
          [] -> assert False undefined -- There is always an inital state
          [init] -> [init]
          _:sr -> sr
    , trace = FTC.rewind trace
    , logTrace =
        case logTrace of
          [] -> []
          _:lr -> lr
    }

-----------------------------------------------------------------------------
--  | Sanitize the simulation
sanitize :: EnvironmentSimulation -> Maybe String
sanitize EnvironmentSimulation {strategy = cst, specification = spec} =
  let specForm = Implies (And $ assumptionsStr spec) (And $ guaranteesStr spec)
      specUpatedCells = getOutputs specForm
      specPredicates = getPredicates specForm
      --
      strategyUpdatedCells =
        fromList $ fmap fst [outputName cst o | o <- outputs cst]
      strategyPredicates = fromList $ [inputName cst o | o <- inputs cst]
      --
      errorMsgCells =
        "Simulator: Specification does not match the strategy as the following cells differ:  " ++
        concatMap
          (++ " ")
          (toList $ difference specUpatedCells strategyUpdatedCells)
      errorMsgPred =
        "Simulator: Specification does not match the strategy as the following predicates differ:  " ++
        concatMap
          (\p -> predicateTermToString id p ++ " ")
          (toList $ difference specPredicates strategyPredicates)
   in case ( specUpatedCells `isSubsetOf` strategyUpdatedCells
           , specPredicates `isSubsetOf` strategyPredicates) of
        (True, True) -> Nothing
        (True, False) -> Just $ errorMsgPred
        (False, True) -> Just $ errorMsgCells
        (False, False) -> Just $ errorMsgCells ++ "\n" ++ errorMsgPred

-----------------------------------------------------------------------------
-- | Get the simulation log
getLog ::
     EnvironmentSimulation
  -> [(EnvironmentOption, [(String, SignalTerm String)])]
getLog = reverse . logTrace

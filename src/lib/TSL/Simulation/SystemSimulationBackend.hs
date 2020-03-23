-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulaton.SystemSimulationBackend
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- The backend of the system simulation when playing againts a counter
-- strategy
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.SystemSimulationBackend
  ( EnvironmentCounterStrategy
  , SystemOption
  , SystemSimulation(..)
  , options
  , step
  , rewind
  , getLog
  , sanitize
  ) where

-----------------------------------------------------------------------------
import Control.Exception (assert)

import TSL.Specification (TSLStringSpecification(..))

import qualified Data.Set as Set (filter)

import Data.Set as Set (difference, fromList, isSubsetOf, map, powerSet, toList)

import TSL.Simulation.AigerSimulator
  ( NormCircuit
  , State
  , inputName
  , inputs
  , latches
  , outputName
  , outputs
  , simStep
  )

import TSL.Simulation.FiniteTraceChecker as FTC (FiniteTrace, append, violated)

import qualified TSL.Simulation.FiniteTraceChecker as FTC (rewind)

import TSL.Logic (Formula(..), PredicateTerm, SignalTerm(..))

import TSL.FormulaUtils (getOutputs, getPredicates)

import TSL.ToString (predicateTermToString)

------------------------------------------------------------------------------
-- | A environment startegy is a circuit with predicate evaluations as outputs
-- and updates as inputs
type EnvironmentCounterStrategy
   = NormCircuit (String, SignalTerm String) (PredicateTerm String)

------------------------------------------------------------------------------
-- | A system simulation consists of the environments counter strategy,
-- the respective specification, the stack of the startegies state, the trace
-- and a logging trace
data SystemSimulation =
  SystemSimulation
    { counterStrategy :: EnvironmentCounterStrategy
    , specification :: TSLStringSpecification
    , stateStack :: [State]
    , trace :: FiniteTrace String
    , logTrace :: [(SystemOption, [(PredicateTerm String, Bool)])]
    }

------------------------------------------------------------------------------
-- | The option of the system is a list of update choice
type SystemOption = [(String, SignalTerm String)]

------------------------------------------------------------------------------
type Witness = [Formula String]

-----------------------------------------------------------------------------
-- | Gives all options of a simulation and a list of TSLFormulas (Witness)
-- that would be violated
options ::
     SystemSimulation
  -> IO [(SystemOption, Witness, [(PredicateTerm String, Bool)])]
options sim@SystemSimulation {counterStrategy = ct} = do
  let options = possibleOptions ct
  steps <- sequence $ fmap (step sim) options
  let witnesses = fmap (violated . trace . fst) steps
  let evaluations = fmap snd steps
  return $ zip3 options witnesses evaluations

possibleOptions :: EnvironmentCounterStrategy -> [SystemOption]
possibleOptions cst =
  Set.toList $ Set.fromList $ fmap extendUpdates filteredCombinations
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

-----------------------------------------------------------------------------
-- | Given an possible action option, simulate one step and calculate the
-- predicate evaluations
--
-- ASSUMPTION: The option should be complete, i.e. on a higher level
-- for every cell in the formula, the circuit can update on of these cells,
-- and the preidcates have to match
-- (can be checked using sanitize)
--
step ::
     SystemSimulation
  -> SystemOption
  -> IO (SystemSimulation, [(PredicateTerm String, Bool)])
step sim@SystemSimulation {..} updates = do
  input <- return $ \i -> elem (inputName counterStrategy i) updates -- The input for the c-strat circuit
  putStrLn $ concatMap (\i -> show $ input i) (inputs counterStrategy)
    --
  (q, output) <- return $ simStep counterStrategy (head stateStack) input -- The c-strat simulation step
  putStrLn $ concatMap (\i -> show $ output i) (outputs counterStrategy)
  putStrLn $ concatMap (\i -> show $ q i) (latches counterStrategy)
    --
    --
  eval <-
    return $
    [(outputName counterStrategy o, output o) | o <- outputs counterStrategy] --The predicate evaluation generated out of the output
    --
  newTrace <-
    return $
    append trace (\c -> findFirst (== c) updates) (\p -> findFirst (== p) eval)
    --
  newLog <- return $ (updates, eval) : logTrace
    --
  return
    ( sim {stateStack = q : stateStack, trace = newTrace, logTrace = newLog}
    , eval)
  where
    findFirst :: (a -> Bool) -> [(a, b)] -> b
    findFirst _ [] = assert False undefined --THIS cant happend iff the simulation is sanitized
    findFirst p ((a, b):xr) =
      if p a
        then b
        else findFirst p xr

-----------------------------------------------------------------------------
-- | Rewind steps the simulation one step back
rewind :: SystemSimulation -> SystemSimulation
rewind sim@SystemSimulation { stateStack = stateStack
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
sanitize :: SystemSimulation -> Maybe String
sanitize SystemSimulation {counterStrategy = cst, specification = spec} =
  let specForm = Implies (And $ assumptionsStr spec) (And $ guaranteesStr spec)
      specUpatedCells = getOutputs specForm
      specPredicates = getPredicates specForm
      --
      strategyUpdatedCells =
        fromList $ fmap fst [inputName cst o | o <- inputs cst]
      strategyPredicates = fromList $ [outputName cst o | o <- outputs cst]
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
getLog :: SystemSimulation -> [(SystemOption, [(PredicateTerm String, Bool)])]
getLog = reverse . logTrace

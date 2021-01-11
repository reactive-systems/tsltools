-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulaton.SystemSimulationBackend
-- Description :  Backend of the system simulation
-- Maintainer  :  Philippe Heim
--
-- This module is the backend of the system simulation when playing
-- against an environment counter-strategy.
--
-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.SymbolTable
  ( stName
  )

import qualified Data.Set as Set
  ( filter
  )

import Data.Set as Set
  ( difference
  , fromList
  , isSubsetOf
  , map
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

import TSL.Simulation.FiniteTraceChecker
  ( FiniteTrace
  , append
  , violated
  )

import qualified TSL.Simulation.FiniteTraceChecker as FTC
  ( rewind
  )

import TSL.Logic
  ( Formula(..)
  , PredicateTerm
  , SignalTerm(..)
  , tslFormula
  , checks
  )

import qualified TSL.Logic as L
  ( outputs
  )

-------------------------------------------------------------------------------
-- | A 'EnvironmentCounterStrategy' is a circuit with predicate evaluations as
-- outputs and updates as inputs.

type EnvironmentCounterStrategy =
  NormCircuit (String, SignalTerm String) (PredicateTerm String)

-------------------------------------------------------------------------------
-- | The playable actions of the system ('SystemOption') are a list of update
-- choices. These are mapping form a cell name to the respective 'SignalTerm'
-- that should update this cell.

type SystemOption = [(String, SignalTerm String)]

-------------------------------------------------------------------------------
-- | A 'Witness' is a list of violated TSL 'Formula's. These are latter used
-- to explain why some option is forbidden.

type Witness = [Formula String]

-------------------------------------------------------------------------------
-- | 'SystemSimulation' is an instance of a game against some environment
-- counter-strategy for some specification.

data SystemSimulation =
  SystemSimulation
    {
    -- | The counter-strategy of the environment played against
      counterStrategy :: EnvironmentCounterStrategy
    -- | The considered specification
    , specification :: Specification
    -- | The history of states that have been passed through as stack, the
    -- topmost element is the current state.
    , stateStack :: [State]
    -- | The trace of updates and predicate evaluation (this trace is the
    -- "trace" that is usually meant in a model checking context)
    , trace :: FiniteTrace String
    -- | The trace of all played actions from the system and the environment
    , logTrace :: [(SystemOption, [(PredicateTerm String, Bool)])]
    }

-------------------------------------------------------------------------------
-- | 'options' computes all options that are potentially playable by the
-- system. Additionally it also computes a 'Witness' of violated TSL
-- 'Formula's and the respective answer of the environment. Note that an
-- option is only 'allowed' if the 'Witness' is empty.

options
  :: SystemSimulation
  -> [(SystemOption, Witness, [(PredicateTerm String, Bool)])]

options sim@SystemSimulation {counterStrategy = ct} =
  let
    options = possibleOptions ct
    steps = fmap (step sim) options
    witnesses = fmap (violated . trace . fst) steps
    evaluations = fmap snd steps
  in
    zip3 options witnesses evaluations

  where
    possibleOptions
      :: EnvironmentCounterStrategy -> [SystemOption]

    possibleOptions cst =
      let
        allUpdates = [inputName cst i | i <- inputs cst]
        cells = removeDoubles $ fmap fst allUpdates
        allCombinations = Set.map toList $ powerSet $ fromList allUpdates
        filteredCombinations =
          toList $ Set.filter (unique . fmap fst) allCombinations
       in
        removeDoubles $
          fmap (removeDoubles . extendUpdates cells) filteredCombinations

    unique = \case
      []   -> True
      c:cr -> (c `notElem` cr) && unique cr

    extendUpdates cells updates =
      foldl
        (\upds c ->
           if all ((/= c) . fst) upds
             then (c, Signal c) : upds
             else upds)
        updates
        cells

    removeDoubles
      :: Ord a => [a] -> [a]

    removeDoubles =
      Set.toList . Set.fromList

-------------------------------------------------------------------------------
-- | 'step' computes the next instance of a 'SystemSimulation' provided a
-- 'SystemOption' played by the system. It also returns the answer
-- of the environment to the system`s move.
--
-- ASSUMPTION: The option should be complete, i.e. for every cell in the
-- specification, the circuit of the system updates one of these cells. This
-- can be check using 'sanitize'. If this is not the case 'step' might fire
-- an assertion error.

step
  :: SystemSimulation -> SystemOption
  -> (SystemSimulation, [(PredicateTerm String, Bool)])

step sim@SystemSimulation {..} updates =
  let
    input = \i -> inputName counterStrategy i `elem` updates

    (q, output) = simStep counterStrategy (head stateStack) input

    eval =
      [ (outputName counterStrategy o, output o)
      | o <- outputs counterStrategy
      ]

    newTrace =
      append
        trace
        (\c -> findFirst (== c) updates)
        (\p -> findFirst (== p) eval)

    newLog = (updates, eval) : logTrace
  in
    ( sim
        { stateStack = q : stateStack
        , trace = newTrace
        , logTrace = newLog
        }
    , eval
    )

  where
    findFirst
      :: (a -> Bool) -> [(a, b)] -> b

    findFirst p = \case
      -- cannot happen if the simulation is sanitized
      []       -> assert False undefined
      -- otherwise
      (a,b):xr
        | p a       -> b
        | otherwise -> findFirst p xr

-------------------------------------------------------------------------------
-- | 'rewind' undoes the last step of applied to a 'SystemSimulation'
-- instance. In the initial state this function is the identity. Note that
-- the 'SystemSimulation' has to be well-defined i.e. there is some current
-- state.

rewind
  :: SystemSimulation -> SystemSimulation

rewind sim@SystemSimulation{..} =
  sim
    { stateStack =
        case stateStack of
          [] -> assert False undefined -- There is always an initial state
          [init] -> [init]
          _:sr -> sr
    , trace = FTC.rewind trace
    , logTrace =
        case logTrace of
          [] -> []
          _:lr -> lr
    }

-------------------------------------------------------------------------------
-- | 'sanitize' checks that a 'SystemSimulation' is consistent. This means that
-- the available predicates and updates in the counter-strategy circuit and
-- the specification match each other.
--
-- TODO: It does not check for the existence of an initial state; maybe it
-- should.

sanitize
  :: SystemSimulation -> Maybe String

sanitize SystemSimulation{counterStrategy = cst, specification = spec} =
  let
    specForm = Implies (And $ assumptionsStr spec) (And $ guaranteesStr spec)
    specUpatedCells = L.outputs specForm
    specPredicates = checks specForm

    strategyUpdatedCells =
      fromList $ fmap fst [inputName cst o | o <- inputs cst]
    strategyPredicates = fromList $ [outputName cst o | o <- outputs cst]

    errorMsgCells =
      "Simulator: Specification does not match the " ++
      "strategy as the following cells differ:  " ++
      concatMap
        (++ " ")
        (toList $ difference specUpatedCells strategyUpdatedCells)

    errorMsgPred =
      "Simulator: Specification does not match the " ++
      "strategy as the following predicates differ:  " ++
      concatMap
        (\p -> tslFormula id (Check p) ++ " ")
        (toList $ difference specPredicates strategyPredicates)
   in
    case ( specUpatedCells `isSubsetOf` strategyUpdatedCells
         , specPredicates `isSubsetOf` strategyPredicates) of
      (True, True) -> Nothing
      (True, False) -> Just errorMsgPred
      (False, True) -> Just errorMsgCells
      (False, False) -> Just $ errorMsgCells ++ "\n" ++ errorMsgPred

  where
    assumptionsStr = fmap (fmap (stName $ symboltable spec)) . assumptions
    guaranteesStr = fmap (fmap (stName $ symboltable spec)) . guarantees

-------------------------------------------------------------------------------
-- | 'getLog' returns the trace of chosen 'SystemOption's and predicate
-- evaluations (chosen by the environment) in the chronological order.

getLog
  :: SystemSimulation -> [(SystemOption, [(PredicateTerm String, Bool)])]

getLog =
  reverse . logTrace

-------------------------------------------------------------------------------

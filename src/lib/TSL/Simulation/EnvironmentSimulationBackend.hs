-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulaton.EnvironmentSimulationBackend
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- The backend of the environment simulation when playing againts a
-- strategy / system
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Simulation.EnvironmentSimulationBackend
  ( SystemStrategy
  , EnvironmentOption
  , EnvironmentSimulation(..)
  , options
  , step
  , rewind
  , getLog
  , sanitize
  ) where

-----------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.SymbolTable
  ( stName
  )

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

-----------------------------------------------------------------------------

-- | A system startegy is a circuit with predicate evaluations as
-- inputs and updates as outputs

type SystemStrategy =
  NormCircuit (PredicateTerm String) (String, SignalTerm String)

-----------------------------------------------------------------------------

-- | The options of the environment is the set of predicates, and the
-- choice for the predicates

type EnvironmentOption =
  (Set (PredicateTerm String), PredicateTerm String -> Bool)

-----------------------------------------------------------------------------

type Witness = [Formula String]

-----------------------------------------------------------------------------

-- | An environemnt simulation consists of the system strategy, the
-- respective specification, the stack of the startegies state, the
-- trace and a logging trace

data EnvironmentSimulation =
  EnvironmentSimulation
    { strategy :: SystemStrategy
    , specification :: Specification
    , stateStack :: [State]
    , trace :: FiniteTrace String
    , logTrace :: [(EnvironmentOption, [(String, SignalTerm String)])]
    }

-----------------------------------------------------------------------------

-- | Gives all options of a simulation and a list of TSLFormulas
-- (Witness) that would be violated

options
  :: EnvironmentSimulation
  -> [(EnvironmentOption, Witness, [(String, SignalTerm String)])]

options sim@EnvironmentSimulation{strategy = ct} =
  zip3 options witnesses evaluations

  where
    options = possibleOptions ct
    steps = fmap (step sim) options
    witnesses = fmap (violated . trace . fst) steps
    evaluations = fmap snd steps

    possibleOptions :: SystemStrategy -> [EnvironmentOption]
    possibleOptions cst =
      let
        allPredicates = [inputName cst i | i <- inputs cst]
        allPredicateChoices = toList $ powerSet $ fromList allPredicates
      in
        fmap
          (\s -> (fromList allPredicates, \p -> p `member` s))
          allPredicateChoices

-----------------------------------------------------------------------------

-- | Given an possible action option, simulate one step and calculate the
-- update choices
--
-- ASSUMPTION: The option should be complete, i.e. on a higher level
-- for every cell in the formula, the circuit can update on of these cells
-- (can be checked using sanitize)

step
  :: EnvironmentSimulation -> EnvironmentOption
  -> (EnvironmentSimulation, [(String, SignalTerm String)])

step sim@EnvironmentSimulation{..} (predicates, predEval) =
  ( sim
      { stateStack = q : stateStack
      , trace = newTrace
      , logTrace = newLog
      }
  , eval
  )

  where
    (q, output) =
      simStep strategy (head stateStack) (predEval . (inputName strategy))

    eval =
      [ outputName strategy o
      | o <- outputs strategy
      , output o
      ]

    newTrace =
      append trace (\c -> findFirst (== c) eval) predEval

    newLog =
      ((predicates, predEval), eval) : logTrace

    findFirst
      :: (a -> Bool) -> [(a, b)] -> b

    findFirst p = \case
      -- can't happend iff the simulation is sanitized
      []       -> assert False undefined
      -- otherwise
      (a,b):xr
        | p a       -> b
        | otherwise ->findFirst p xr

-----------------------------------------------------------------------------

-- | Rewind steps the simulation one step back

rewind
  :: EnvironmentSimulation -> EnvironmentSimulation

rewind sim@EnvironmentSimulation{..} =
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

-- | Sanitize the simulation

sanitize
  :: EnvironmentSimulation -> Maybe String

sanitize EnvironmentSimulation{strategy = cst, specification = spec} =
  let
    specForm = Implies (And $ assumptionsStr spec) (And $ guaranteesStr spec)
    specUpatedCells = L.outputs specForm
    specPredicates = checks specForm

    strategyUpdatedCells =
      fromList $ fmap fst [outputName cst o | o <- outputs cst]
    strategyPredicates = fromList $ [inputName cst o | o <- inputs cst]

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
      (True, False) -> Just $ errorMsgPred
      (False, True) -> Just $ errorMsgCells
      (False, False) -> Just $ errorMsgCells ++ "\n" ++ errorMsgPred

  where
    assumptionsStr = fmap (fmap (stName $ symboltable spec)) . assumptions
    guaranteesStr = fmap (fmap (stName $ symboltable spec)) . guarantees

-----------------------------------------------------------------------------

-- | Get the simulation log

getLog
  :: EnvironmentSimulation
  -> [(EnvironmentOption, [(String, SignalTerm String)])]

getLog =
  reverse . logTrace

-----------------------------------------------------------------------------

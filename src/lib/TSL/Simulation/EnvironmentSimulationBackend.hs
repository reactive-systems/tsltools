-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulaton.EnvironmentSimulationBackend
-- Description :  Backend of the environment simulation
-- Maintainer  :  Philippe Heim
--
-- This module is the backend of the environment simulation when playing 
-- against a system strategy.
--
-------------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- | A 'SystemStrategy' is a circuit with predicate evaluations as
-- inputs and updates as outputs.

type SystemStrategy =
  NormCircuit (PredicateTerm String) (String, SignalTerm String)


-------------------------------------------------------------------------------
-- | The playable actions of the environment ('EnvironmentOption') are a set
-- of predicates that should be set and their respective evaluations.

type EnvironmentOption =
  (Set (PredicateTerm String), PredicateTerm String -> Bool)

-------------------------------------------------------------------------------
-- | A 'Witness' is a list of violated TSL 'Formula's. These are latter used
-- to explain why some option is forbidden.
type Witness = [Formula String]

------------------------------------------------------------------------------ 
-- | 'EnvironmentSimulation' is an instance of a game against some system 
-- strategy for some specification.

data EnvironmentSimulation =
  EnvironmentSimulation
    -- | The strategy of the system played against
    { strategy :: SystemStrategy
    -- | The considered specification
    , specification :: Specification
    -- | The history of states that have been passed through as stack, the
    -- topmost element is the current state.
    , stateStack :: [State]
    -- | The trace of updates and predicate evaluation (this trace is the 
    -- "trace" that is usually meant in a model checking context)
    , trace :: FiniteTrace String
    -- | The trace of all played actions from the system and the environment
    , logTrace :: [(EnvironmentOption, [(String, SignalTerm String)])]
    }

-------------------------------------------------------------------------------
-- | 'options' computes all options that are potentially playable by the 
-- environment. Additionally it also computes a 'Witness' of violated TSL 
-- 'Formula's and the respective answer of the system. Note that an option 
-- is only 'allowed' if the 'Witness' is empty.

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
          (\s -> (fromList allPredicates, (`member` s)))
          allPredicateChoices

-------------------------------------------------------------------------------
-- | 'step' computes the next instance of an 'EnvironmentSimulation' provided
-- an 'EnvironmentOption' played by the environment. It also returns the answer
-- of the system to the environment`s move.
--
-- ASSUMPTION: The option should be complete, i.e. for every cell in the 
-- specification, the circuit of the system updates one of these cells. This 
-- can be check using 'sanitize'. If this is not the case 'step' might fire
-- an assertion error.

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
      simStep strategy (head stateStack) (predEval . inputName strategy)

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
      -- This cannot happen if the simulation is sanitized
      []       -> assert False undefined
      -- otherwise
      (a,b):xr
        | p a       -> b
        | otherwise ->findFirst p xr

-------------------------------------------------------------------------------
-- | 'rewind' undoes the last step of applied to an 'EnvironmentSimulation'
-- instance. In the initial state this function is the identity. Note that
-- the 'EnviromentSimulation' has to be well-defined i.e. there is some current
-- state.

rewind
  :: EnvironmentSimulation -> EnvironmentSimulation

rewind sim@EnvironmentSimulation{..} =
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
-- | 'sanitize' checks that an 'EnvironmentSimulation' is consistent. This 
-- means that the available predicates and updates in the system circuit and
-- the specification match each other. 
--
-- TODO: It does not check for the existence of an initial state; maybe it 
-- should.

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
      (True, False) -> Just errorMsgPred
      (False, True) -> Just errorMsgCells
      (False, False) -> Just $ errorMsgCells ++ "\n" ++ errorMsgPred

  where
    assumptionsStr = fmap (fmap (stName $ symboltable spec)) . assumptions
    guaranteesStr = fmap (fmap (stName $ symboltable spec)) . guarantees

-------------------------------------------------------------------------------
-- | 'getLog' returns the trace of chosen 'EnvironmentOption's and updates 
-- (chosen by the system) in the chronological order.

getLog
  :: EnvironmentSimulation
  -> [(EnvironmentOption, [(String, SignalTerm String)])]

getLog =
  reverse . logTrace

-------------------------------------------------------------------------------

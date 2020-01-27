-- |
-- Module      :  TSL.Simulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulator
  ( Simulation
  , Option
  , createSimulation
  , step
  , TSL.Simulator.rewind
  , options
  , getLog
  ) where

-----------------------------------------------------------------------------
import Data.List as List (filter)
import Data.Set as Set
  ( Set
  , difference
  , empty
  , filter
  , fromList
  , isSubsetOf
  , map
  , powerSet
  , toList
  , unions
  )
import TSL.Aiger (Circuit)

import TSL.Simulator.Core
  ( NormCircuit(inputName, inputs, outputName, outputs)
  , State
  , normalize
  , simStep
  )

import TSL.Logic
  ( Formula(..)
  , PredicateTerm
  , SignalTerm(..)
  , readInput
  , readOutput
  )

import TSL.Error (Error)

import TSL.Specification
  ( TSLSpecification
  , TSLStringSpecification(..)
  , tslSpecToTSLStrSpec
  )

import TSL.Simulator.FiniteTraceChecker as FTC
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
    , logTrace :: [(Option, [(PredicateTerm String, Bool)])]
    }

type Option = [(String, SignalTerm String)]

type Witness = [Formula String]

-----------------------------------------------------------------------------
--
-- Given a aag counterstrategy and a correspoding tsl spec
-- generates a simulation out of this
--
createSimulation :: Circuit -> TSLSpecification -> Simulation
createSimulation aag spec =
  case sanitize sim of
    Nothing -> sim
    Just err -> error err
  where
    parseAP :: (String -> Either Error a) -> String -> a
    parseAP parse str =
      case parse str of
        Left err -> error $ "Simulator: While parsing ap found " ++ (show err)
        Right val -> val
    --
    sim =
      Simulation
        { counterStrategy =
            normalize (parseAP readOutput) (parseAP readInput) aag
        , specification = tslSpecToTSLStrSpec spec
        , stateStack = [\_ -> False]
        , trace = emptyTrace
        , logTrace = []
        }

-----------------------------------------------------------------------------
--
-- Gives all options of a simulation and a list of TSLFormulas (Witness)
-- that would be violated
--
options :: Simulation -> [(Option, Witness, [(PredicateTerm String, Bool)])]
options sim@Simulation {counterStrategy = ct, specification = spec} =
  zip3 options witnesses evaluations
  where
    options = possibleOptions ct
    steps = fmap (step sim) options
    witnesses = fmap ((violatedGuarantees spec) . trace . fst) steps
    evaluations = fmap snd steps

possibleOptions :: CounterStrategy -> [Option]
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
  (sim {stateStack = q : stateStack, trace = newTrace, logTrace = newLog}, eval)
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
    newLog = (updates, eval) : logTrace
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
rewind sim@Simulation { stateStack = stateStack
                      , trace = trace
                      , logTrace = logTrace
                      } =
  sim
    { stateStack =
        case stateStack of
          [] ->
            error
              "Simulator: Assertion, there should always be the initial state"
          [init] -> [init]
          _:sr -> sr
    , trace = FTC.rewind trace
    , logTrace =
        case logTrace of
          [] -> []
          _:lr -> lr
    }

-----------------------------------------------------------------------------
--
-- Sanitize the simulation
--
sanitize :: Simulation -> Maybe String
sanitize Simulation {counterStrategy = cst, specification = spec} =
  if isSubsetOf cellsSpec cellsStrat
    then Nothing
    else Just $
         "Simulator: Specification does noz match counter-strategy as the following cells differ:  " ++
         concatMap (++ " ") (toList $ difference cellsSpec cellsStrat)
  where
    allUpdates = [inputName cst i | i <- inputs cst]
    cellsStrat = fromList $ fmap fst allUpdates
    -- 
    specForm = Implies (And $ assumptionsStr spec) (And $ guaranteesStr spec)
    cellsSpec = formulaUpdCells specForm
    --
    formulaUpdCells :: Formula String -> Set String
    formulaUpdCells =
      \case
        Check _ -> empty
        Update c _ -> fromList [c]
        Not f -> formulaUpdCells f
        Implies f1 f2 -> unions [formulaUpdCells f1, formulaUpdCells f2]
        Equiv f1 f2 -> unions [formulaUpdCells f1, formulaUpdCells f2]
        And fs -> unions $ fmap formulaUpdCells fs
        Or fs -> unions $ fmap formulaUpdCells fs
        Next f -> formulaUpdCells f
        Globally f -> formulaUpdCells f
        Finally f -> formulaUpdCells f
        Until f1 f2 -> unions [formulaUpdCells f1, formulaUpdCells f2]
        Release f1 f2 -> unions [formulaUpdCells f1, formulaUpdCells f2]
        Weak f1 f2 -> unions [formulaUpdCells f1, formulaUpdCells f2]
        _ -> empty

-----------------------------------------------------------------------------
--
-- Get the simulation log
--
getLog :: Simulation -> [(Option, [(PredicateTerm String, Bool)])]
getLog = reverse . logTrace

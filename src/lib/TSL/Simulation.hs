-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple counterstrategy simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation
  ( Simulation
  , Option
  , createSimulation
  , step
  , rewind
  , options
  , getLog
  , simulate
  ) where

-----------------------------------------------------------------------------
import TSL.Reader (fromTSLtoTSLSpec)

import TSL.ToString (formulaToString, predicateTermToString, signalTermToString)

import Text.Read (readMaybe)

import TSL.Error (genericError)

import qualified Data.List as List (filter)

import qualified Data.Set as Set (filter)

import Control.Exception (assert)

import Data.Set as Set
  ( Set
  , difference
  , empty
  , fromList
  , isSubsetOf
  , map
  , powerSet
  , toList
  , unions
  )

import TSL.Aiger (Circuit, parseAag)

import TSL.Simulation.Core
  ( NormCircuit
  , State
  , inputName
  , inputs
  , normalize
  , outputName
  , outputs
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

import TSL.Simulation.FiniteTraceChecker as FTC
  ( FiniteTrace
  , (|=)
  , append
  , emptyTrace
  )

import qualified TSL.Simulation.FiniteTraceChecker as FTC (rewind)

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
-- | Generates a simulation out of a given an AIGER counterstrategy
-- and a correspoding TSL specification.
createSimulation :: Circuit -> TSLSpecification -> Either Error Simulation
createSimulation aag spec =
  case normalize readOutput readInput aag of
    Left err -> Left err
    Right naag ->
      let sim =
            Simulation
              { counterStrategy = naag
              , specification = tslSpecToTSLStrSpec spec
              , stateStack = [\_ -> False]
              , trace = emptyTrace
              , logTrace = []
              }
       in case sanitize sim of
            Nothing -> Right sim
            Just err -> genericError $ err

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
--  (can be checked using sanitize)
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
        (\c -> findFirst (== c) updates)
        (\p -> findFirst (== p) eval)
    --
    newLog = (updates, eval) : logTrace
    --
    findFirst :: (a -> Bool) -> [(a, b)] -> b
    findFirst _ [] = assert False undefined --THIS cant happend iff the simulation is sanitized
    findFirst p ((a, b):xr) =
      if p a
        then b
        else findFirst p xr

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

---------------------------------------------------------------------------
--
-- The possible actions a user can take
--
data Action
  = Stop
  | Rewind
  | Opt Option
  | ShowWhyOthersNot
  | ShowTrace

---------------------------------------------------------------------------
--
-- Gets the action a user may take (includes input sanitizing)
--
getUserInput :: [Option] -> IO Action
getUserInput possibleOptions = do
  putStrLn $
    "Your turn now:\n" ++
    "  s:  Give up\n" ++
    "  r:  Rewind one step\n" ++
    " <n>: Choose option n\n" ++
    "  o:  Show why other options are not possible\n" ++
    "  t:  Show the trace till now\n"
  inpt <- getLine
  case inpt of
    "r" -> return Rewind
    "s" -> return Stop
    "o" -> return ShowWhyOthersNot
    "t" -> return ShowTrace
    inp ->
      case readMaybe inp of
        Just num ->
          if num >= 0 && num < length possibleOptions
            then return $ Opt $ (possibleOptions !! num)
            else do
              putStrLn "Not a valid option"
              getUserInput possibleOptions
        Nothing -> do
          putStrLn "Not a valid command"
          getUserInput possibleOptions

---------------------------------------------------------------------------
--
-- Runs a simulation
--
runSimulation :: Simulation -> IO ()
runSimulation sim = do
  let opts = options sim
  let posOpts = fmap (\(v, _, _) -> v) $ filter (\(_, xs, _) -> null xs) opts
  let imposOpts = filter (\(_, xs, _) -> not $ null xs) opts
  putStrLn "Your options are:"
  putStrLn $
    snd $
    foldl
      (\(n, xs) e ->
         (n + 1, xs ++ "  " ++ show n ++ " " ++ optionToString e ++ "\n"))
      (0 :: Int, [])
      posOpts
  act <- getUserInput posOpts
  case act of
    Stop -> return ()
    Rewind -> runSimulation (rewind sim)
    ShowWhyOthersNot -> do
      putStr $ concatMap optionWitnessToString imposOpts
      runSimulation sim
    Opt opt ->
      let (sim', preds) = step sim opt
       in do putStrLn "\nThe environment chooses:"
             putStrLn (predicateEvalsToString preds)
             putStrLn ""
             runSimulation sim'
    ShowTrace -> do
      putStrLn $
        "########\n" ++
        (concatMap
           (\(o, p) ->
              optionToString o ++
              "\n---------\n" ++ predicateEvalsToString p ++ "\n") $
         getLog sim) ++
        "########\n"
      runSimulation sim

---------------------------------------------------------------------------
--
-- Different sub-printing methods
--
optionToString :: Option -> String
optionToString [] = ""
optionToString ((c, st):xr) =
  "[" ++ c ++ " <- " ++ signalTermToString id st ++ "] " ++ optionToString xr

predicateEvalsToString :: [(PredicateTerm String, Bool)] -> String
predicateEvalsToString =
  concatMap
    (\(pt, v) ->
       (if v
          then "   "
          else "not") ++
       predicateTermToString id pt ++ "\n")

optionWitnessToString ::
     (Option, [Formula String], [(PredicateTerm String, Bool)]) -> String
optionWitnessToString (o, fs, predEvals) =
  (optionToString o) ++
  " is impossible as the environment would choose\n" ++
  predicateEvalsToString predEvals ++
  " and then each of these guarantees would be violated\n" ++
  concatMap (\f -> "    " ++ formulaToString id f ++ "\n") fs ++ " \n"

---------------------------------------------------------------------------
-- | Given the filepaths of the tsl file and the agg file, load and
-- run the simulation
simulate :: String -> String -> Either Error (IO ())
simulate tsl' aag' = do
  aag <- parseAag aag'
  tsl <- fromTSLtoTSLSpec tsl'
  case createSimulation aag tsl of
    Left err -> Left $ err
    Right sim -> Right $ runSimulation sim

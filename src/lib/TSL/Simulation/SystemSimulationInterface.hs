-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.SystemSimulationInterface
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- The (command line) interface of the system simulation when playing againts a counter
-- strategy
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.SystemSimulationInterface
  ( runSimulation
  ) where

-----------------------------------------------------------------------------
import TSL.ToString (formulaToString, predicateTermToString, signalTermToString)

import Text.Read (readMaybe)

import TSL.Simulation.SystemSimulationBackend
  ( SystemOption
  , SystemSimulation(..)
  , getLog
  , options
  , rewind
  , step
  )

import TSL.Logic (Formula, PredicateTerm)

---------------------------------------------------------------------------
--
-- The possible actions a user can take
--
data Action
  = Stop
  | Rewind
  | Opt SystemOption
  | ShowWhyOthersNot
  | ShowTrace

---------------------------------------------------------------------------
--
-- Gets the action a user may take (includes input sanitizing)
--
getUserInput :: [SystemOption] -> IO Action
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
runSimulation :: SystemSimulation -> IO ()
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
optionToString :: SystemOption -> String
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
     (SystemOption, [Formula String], [(PredicateTerm String, Bool)]) -> String
optionWitnessToString (o, fs, predEvals) =
  (optionToString o) ++
  " is impossible as the environment would choose\n" ++
  predicateEvalsToString predEvals ++
  " and then each of these guarantees would be violated\n" ++
  concatMap (\f -> "    " ++ formulaToString id f ++ "\n") fs ++ " \n"

-- |
-- Module      :  Simulator.CLI
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple cli for the simulator backend
--
-- TODO 
--  > Sanitize stuff
--  > Trace priting
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

---------------------------------------------------------------------------
module Simulator.CLI where

---------------------------------------------------------------------------
import Simulator.Simulator as Simulator
  ( Option
  , Simulation
  , createSimulation
  , options
  , rewind
  , step
  )

import TSL.Aiger (parseAag)
import TSL.Logic (Formula, PredicateTerm)
import TSL.Reader (fromTSLtoTSLSpec)
import TSL.ToString (formulaToString, predicateTermToString, signalTermToString)

import Text.Read (readMaybe)

---------------------------------------------------------------------------
--
-- Loads the simulation given the necessary file path (tsl, agg) 
--
loadSimulation :: FilePath -> FilePath -> IO Simulation
loadSimulation pathAag pathSpec = do
  putStrLn
    "\nWARNING: The backend still assumes matching counterstartegy and specification !!\n"
  specOut <- readFile pathSpec
  aagOut <- readFile pathAag
  let aag =
        case parseAag aagOut of
          Left err ->
            error $ "Simulator.CLI: While loading aag File found " ++ (show err)
          Right a -> a
  let spec =
        case fromTSLtoTSLSpec specOut of
          Left err ->
            error $
            "Simulator.CLI: While loading specification file found " ++
            (show err)
          Right a -> a
  return $ createSimulation aag spec

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
    "  s: Give up\n" ++
    "  r: Rewind one step\n" ++
    " <n>: Choose option n\n" ++
    "  o: Show why other options are not possible\n" ++
    "  t: Show the trace till now\n"
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
      (0, [])
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
    ShowTrace -> error "NOT SUPPORTED YET"

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
          else " not") ++
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
--
-- Given the filepaths of the tsl file and the agg file, load
-- and run the simulation
--
simulate :: String -> String -> IO ()
simulate tslPath aagPath = do
  sim <- loadSimulation aagPath tslPath
  runSimulation m

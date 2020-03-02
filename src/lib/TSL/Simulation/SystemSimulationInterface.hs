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
import TSL.ToString (formulaToString)

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

import TSL.Simulation.InterfacePrintingUtils
  ( Color(..)
  , cPutStr
  , cPutStrLn
  , initInterface
  , predicateEvaluationListToString
  , resetInterface
  , updateListToString
  )

---------------------------------------------------------------------------
-- | The possible actions a user can take
data Action
  = Stop
  | Rewind
  | Opt SystemOption
  | ShowWhyOthersNot

---------------------------------------------------------------------------
-- | Gets the action a user may take (includes input sanitizing)
getUserInput :: [SystemOption] -> IO Action
getUserInput possibleOptions = do
  cPutStrLn White $
    "Your turn now:\n" ++
    "  s:  Give up\n" ++
    "  r:  Rewind one step\n" ++
    " <n>: Choose option n\n" ++
    "  o:  Show why other options are not possible\n"
  cPutStrLn Red "Choose: "
  inpt <- getLine
  case inpt of
    "r" -> return Rewind
    "s" -> return Stop
    "o" -> return ShowWhyOthersNot
    inp ->
      case readMaybe inp of
        Just num ->
          if num >= 0 && num < length possibleOptions
            then return $ Opt $ (possibleOptions !! num)
            else do
              cPutStrLn Red "Not a valid option\n"
              getUserInput possibleOptions
        Nothing -> do
          cPutStrLn Red "Not a valid command\n"
          getUserInput possibleOptions

---------------------------------------------------------------------------
-- | Runs a simulation
runSimulation :: SystemSimulation -> IO ()
runSimulation sim = do
  initInterface
  let opts = options sim
  let posOpts = fmap (\(v, _, _) -> v) $ filter (\(_, xs, _) -> null xs) opts
  let imposOpts = filter (\(_, xs, _) -> not $ null xs) opts
  cPutStrLn White "Your options are:"
  cPutStrLn Magenta $
    (snd $
     foldl
       (\(n, xs) e ->
          (n + 1, xs ++ "  " ++ show n ++ " " ++ updateListToString e ++ "\n"))
       (0 :: Int, [])
       posOpts)
  act <- getUserInput posOpts
  case act of
    Stop -> do
      cPutStrLn
        Red
        "You gave up! The environment seems to be stronger than you."
      return ()
    Rewind -> do
      let sim' = rewind sim
      resetInterface
      printTrace sim'
      putStrLn ""
      cPutStrLn Red "You steped on step back.\n"
      runSimulation sim'
    ShowWhyOthersNot -> do
      resetInterface
      printTrace sim
      putStrLn ""
      cPutStrLn White $ concatMap optionWitnessToString imposOpts
      runSimulation sim
    Opt opt -> do
      let (sim', _) = step sim opt
      resetInterface
      printTrace sim'
      putStrLn ""
      runSimulation sim'

printTrace :: SystemSimulation -> IO ()
printTrace sim = do
  let log = getLog sim
  _ <-
    sequence $
    map
      (\(opt, preds) -> do
         cPutStr Cyan "You (System): "
         cPutStrLn White (updateListToString opt)
         cPutStr Cyan "Environment:  "
         cPutStrLn White (predicateEvaluationListToString preds))
      log
  return ()

optionWitnessToString ::
     (SystemOption, [Formula String], [(PredicateTerm String, Bool)]) -> String
optionWitnessToString (o, fs, predEvals) =
  (updateListToString o) ++
  " is impossible as the environment would choose\n" ++
  predicateEvaluationListToString predEvals ++
  " and then each of these guarantees would be violated\n" ++
  concatMap (\f -> "    " ++ formulaToString id f ++ "\n") fs ++ " \n"

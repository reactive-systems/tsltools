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

import TSL.Specification (TSLStringSpecification(..))

import TSL.Simulation.FiniteTraceChecker (nextObligation)

import TSL.Logic (Formula(And), PredicateTerm)

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
  resetInterface
  execSimulation sim

execSimulation :: SystemSimulation -> IO ()
execSimulation sim = do
  let opts = options sim
  let posOpts = fmap (\(v, _, _) -> v) $ filter (\(_, xs, _) -> null xs) opts
  let imposOpts = filter (\(_, xs, _) -> not $ null xs) opts
  cPutStrLn Magenta "Your options are:"
  cPutStrLn White $
    (snd $
     foldl
       (\(n, xs) e ->
          (n + 1, xs ++ "  " ++ show n ++ " " ++ updateListToString e ++ "\n"))
       (0 :: Int, [])
       posOpts)
  cPutStrLn Magenta $
    "Your turn now:\n" ++
    "  s:  Give up\n" ++
    "  r:  Rewind one step\n" ++
    " <n>: Choose option n\n" ++
    "  o:  Show why other options are not possible\n"
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
      execSimulation sim'
    ShowWhyOthersNot -> do
      resetInterface
      printTrace sim
      putStrLn ""
      _ <- sequence $ map printImpossibleOptions imposOpts
      execSimulation sim
    Opt opt -> do
      let (sim', _) = step sim opt
      resetInterface
      printTrace sim'
      putStrLn ""
      execSimulation sim'

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
  _ <-
    sequence $
    map
      (\c ->
         cPutStrLn Red $
         " DEBUG: " ++ (formulaToString id (nextObligation (trace sim) c)))
      (guaranteesStr $ specification sim)
  _ <-
    cPutStrLn Red $
    " DEBUG: " ++
    (formulaToString
       id
       (nextObligation (trace sim) (And $ assumptionsStr $ specification sim)))
  return ()

printImpossibleOptions ::
     (SystemOption, [Formula String], [(PredicateTerm String, Bool)]) -> IO ()
printImpossibleOptions (opt, fs, predEvals) = do
  cPutStrLn White (updateListToString opt)
  cPutStrLn Magenta "is impossible as the environemnt would choose"
  cPutStrLn White (predicateEvaluationListToString predEvals)
  cPutStrLn Magenta "and then each of these guarantees would be violated"
  cPutStrLn White $
    concatMap (\f -> "> " ++ formulaToString id f ++ "\n") fs ++ " \n"

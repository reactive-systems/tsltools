-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.EnvironmentSimulationInterface
-- Description :  TUI for the environment simulation
-- Maintainer  :  Philippe Heim
--
-- This module provides an interactive TUI for the environment simulation.
--
-------------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-------------------------------------------------------------------------------

module TSL.Simulation.EnvironmentSimulationInterface
  ( runSimulation
  ) where

-------------------------------------------------------------------------------

import Text.Read
  ( readMaybe
  )

import TSL.Simulation.EnvironmentSimulationBackend
  ( EnvironmentOption
  , EnvironmentSimulation(..)
  , getLog
  , options
  , rewind
  , step
  )

import TSL.Logic
  ( Formula
  , SignalTerm
  , tslFormula
  )

import TSL.Simulation.InterfacePrintingUtils
  ( Color(..)
  , cPutStr
  , cPutStrLn
  , initInterface
  , mappedPredicateEvaluationToString
  , resetInterface
  , updateListToString
  )

-------------------------------------------------------------------------------
-- | 'Action' describes the possible actions a user can take.

data Action
  -- | Stop the simulation
  = Stop
  -- | Undo one step
  | Rewind
  -- | Choose a system input
  | Opt EnvironmentOption
  -- | Show why other system outputs are not possible (i.e. what they violate)
  | ShowWhyOthersNot

-------------------------------------------------------------------------------
-- | 'getUserInput' retrieves the action chosen by the user. Therefore a
-- overview over the possible actions is printed and how they are performed.
-- Also the input is sanitized and re-queried if invalid.

getUserInput :: [EnvironmentOption] -> IO Action
getUserInput possibleOptions = do
  cPutStrLn Red "Choose: "
  getLine >>= \case
    "r" -> return Rewind
    "s" -> return Stop
    "o" -> return ShowWhyOthersNot
    inp ->
      case readMaybe inp of
        Just num ->
          if num >= 0 && num < length possibleOptions
          then return $ Opt (possibleOptions !! num)
          else do
            cPutStrLn Red "Not a valid option\n"
            getUserInput possibleOptions
        Nothing -> do
          cPutStrLn Red "Not a valid command\n"
          getUserInput possibleOptions

-------------------------------------------------------------------------------
-- | 'runSimulation' preforms an interactive system simulation

runSimulation :: EnvironmentSimulation -> IO ()
runSimulation sim = do
  initInterface
  resetInterface
  execSimulation sim

  where
    execSimulation
      :: EnvironmentSimulation -> IO ()

    execSimulation sim = do
      let
        opts = options sim
        posOpts = (\(v, _, _) -> v) <$> filter (\(_, xs, _) -> null xs) opts
        imposOpts = filter (\(_, xs, _) -> not $ null xs) opts

      cPutStrLn Magenta "Your options are:"
      cPutStrLn White
        (snd $
         foldl
           (\(n, xs) e ->
              ( n + 1
              , xs ++
                "  " ++ show n ++ " " ++
                mappedPredicateEvaluationToString e ++ "\n"
              ))
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
          cPutStrLn Red $ "You gave up! The system seems " ++
            "to be stronger than you."
          return ()
        Rewind -> do
          let sim' = rewind sim
          resetAndPrint sim'
          cPutStrLn Red "You stepped one step back.\n"
          execSimulation sim'
        ShowWhyOthersNot -> do
          resetAndPrint sim
          mapM_ printImpossibleOptions imposOpts
          execSimulation sim
        Opt opt -> do
          let (sim', _) = step sim opt
          resetAndPrint sim'
          execSimulation sim'

    resetAndPrint
      :: EnvironmentSimulation -> IO()
    resetAndPrint sim = do
      resetInterface
      printTrace sim
      putStrLn ""

    printTrace
      :: EnvironmentSimulation -> IO ()
    printTrace sim = do
      let log = getLog sim
      mapM_
          (\(opt, updates) -> do
             cPutStr Cyan "You (Environment): "
             cPutStrLn White (mappedPredicateEvaluationToString opt)
             cPutStr Cyan "System:            "
             cPutStrLn White (updateListToString updates))
          log

    printImpossibleOptions
      :: ( EnvironmentOption
         , [Formula String]
         , [(String, SignalTerm String)]
         ) -> IO ()

    printImpossibleOptions (opt, fs, updEvals) = do
      cPutStrLn White (mappedPredicateEvaluationToString opt)
      cPutStrLn Magenta "is impossible as the environment would choose"
      cPutStrLn White (updateListToString updEvals)
      cPutStrLn Magenta
        "and then each of these guarantees would be violated"
      cPutStrLn White $
        concatMap (\f -> "> " ++ tslFormula id f ++ "\n") fs ++ " \n"

-------------------------------------------------------------------------------

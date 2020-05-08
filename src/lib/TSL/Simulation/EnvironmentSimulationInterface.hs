-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.EnvironmentSimulationInterface
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- The (command line) interface of the environment simulation when
-- playing againts a strategy
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Simulation.EnvironmentSimulationInterface
  ( runSimulation
  ) where

-----------------------------------------------------------------------------

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

---------------------------------------------------------------------------

-- | The possible actions a user can take

data Action
  = Stop
  | Rewind
  | Opt EnvironmentOption
  | ShowWhyOthersNot

---------------------------------------------------------------------------

-- | Gets the action a user may take (includes input sanitizing)

getUserInput
  :: [EnvironmentOption] -> IO Action

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
          then return $ Opt $ (possibleOptions !! num)
          else do
            cPutStrLn Red "Not a valid option\n"
            getUserInput possibleOptions
        Nothing -> do
          cPutStrLn Red "Not a valid command\n"
          getUserInput possibleOptions

---------------------------------------------------------------------------

-- | Runs a simulation

runSimulation
  :: EnvironmentSimulation -> IO ()

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
        posOpts = fmap (\(v, _, _) -> v)
                    $ filter (\(_, xs, _) -> null xs) opts
        imposOpts = filter (\(_, xs, _) -> not $ null xs) opts

      cPutStrLn Magenta "Your options are:"
      cPutStrLn White $
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

    printTrace
      :: EnvironmentSimulation -> IO ()

    printTrace sim = do
      let log = getLog sim
      _ <-
        sequence $
        map
          (\(opt, updates) -> do
             cPutStr Cyan "You (Environment): "
             cPutStrLn White (mappedPredicateEvaluationToString opt)
             cPutStr Cyan "System:            "
             cPutStrLn White (updateListToString updates))
          log
      return ()

    printImpossibleOptions
      :: ( EnvironmentOption
         , [Formula String]
         , [(String, SignalTerm String)]
         ) -> IO ()

    printImpossibleOptions (opt, fs, updEvals) = do
      cPutStrLn White (mappedPredicateEvaluationToString opt)
      cPutStrLn Magenta "is impossible as the environemnt would choose"
      cPutStrLn White (updateListToString updEvals)
      cPutStrLn Magenta
        "and then each of these guarantees would be violated"
      cPutStrLn White $
        concatMap (\f -> "> " ++ tslFormula id f ++ "\n") fs ++ " \n"

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.SystemSimulationInterface
-- Maintainer  :  Philippe Heim
--
-- The (command line) interface of the system simulation when playing
-- againts a counter strategy
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ViewPatterns
  , LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Simulation.SystemSimulationInterface
  ( runSimulation
  ) where

-----------------------------------------------------------------------------

import Text.Read
  ( readMaybe
  )

import TSL.Simulation.SystemSimulationBackend
  ( SystemOption
  , SystemSimulation(..)
  , getLog
  , options
  , rewind
  , step
  )

import TSL.Simulation.FiniteTraceChecker
  ( Obligation(expGuarantee)
  , nextObligations
  )

import TSL.Logic
  ( Formula
  , PredicateTerm
  , tslFormula
  )

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

getUserInput
  :: [SystemOption] -> IO Action

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
  :: SystemSimulation -> IO ()

runSimulation sim = do
  initInterface
  resetInterface
  printTrace sim
  cPutStrLn White ""
  execSimulation sim

  where
    execSimulation
      :: SystemSimulation -> IO ()

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
              (n + 1, xs ++ "  " ++ show n ++
                      " " ++ updateListToString e ++ "\n"))
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
          cPutStrLn Red "You stepped one step back.\n"
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
      :: SystemSimulation -> IO ()

    printTrace sim = do
      let log = getLog sim
      sequence_ $
        map
          (\(opt, preds) -> do
             cPutStr Cyan "You (System): "
             cPutStrLn White (updateListToString opt)
             cPutStr Cyan "Environment:  "
             cPutStrLn White (predicateEvaluationListToString preds))
          log
      cPutStrLn White ""
      cPutStrLn Magenta "Your obligations are:"
      sequence_ $
        map
          (\o -> cPutStrLn White
                  ("> " ++ tslFormula id (expGuarantee o)))
          (nextObligations (trace sim))
      return ()

    printImpossibleOptions
      :: (SystemOption, [Formula String], [(PredicateTerm String, Bool)])
      -> IO ()

    printImpossibleOptions (opt, fs, predEvals) = do
      cPutStrLn White (updateListToString opt)
      cPutStrLn Magenta "is impossible as the environment would choose"
      cPutStrLn White (predicateEvaluationListToString predEvals)
      cPutStrLn Magenta
        "and then each of these guarantees would be violated"
      cPutStrLn White $
        concatMap (\f -> "> " ++ tslFormula id f ++ "\n") fs ++ " \n"

-----------------------------------------------------------------------------

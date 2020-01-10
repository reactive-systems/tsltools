-- |
-- Module      :  Simulator.CLI
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple cli for the simulator backend
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

---------------------------------------------------------------------------
module Simulator.CLI where

---------------------------------------------------------------------------
--
-- TODO: Handle multiple same options, sanitize stuff, initial environment choice ???
--
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

---------------------------------------------------------------------------
loadSimulation :: FilePath -> FilePath -> IO Simulation
loadSimulation pathAag pathSpec = do
  putStrLn
    "WARNING: The backend still assumes matching counterstartegies and specification"
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
data Action
  = Stop
  | Rewind
  | Opt Option

getAction :: Simulation -> IO Action
getAction sim = do
  let opts = options sim
  let posOpts = fmap fst $ filter (\(_, xs) -> null xs) opts
  let imposOpts = filter (\(_, xs) -> not $ null xs) opts
  putStr $ concatMap optionWitnessToString imposOpts
  putStrLn "Your options are:"
  putStrLn $
    snd $
    foldl
      (\(n, xs) e ->
         (n + 1, xs ++ "  " ++ show n ++ " " ++ optionToString e ++ "\n"))
      (0, [])
      posOpts
  putStrLn $
    "Your turn now:\n" ++
    "  S: Give up\n" ++ "  R: Rewind one step\n" ++ " <n>: Choose option n"
  inpt <- getLine
  case inpt of
    "R" -> return Rewind
    "S" -> return Stop
    inp ->
      let opt = posOpts !! (read inp) --TODO Sanitize for invalid user input
       in return $ Opt opt

---------------------------------------------------------------------------
runSimulation :: Simulation -> IO ()
runSimulation sim = do
  act <- getAction sim
  case act of
    Stop -> return ()
    Rewind -> runSimulation (rewind sim)
    Opt opt ->
      let (sim', preds) = step sim opt
       in do printPredicates preds
             runSimulation sim'

---------------------------------------------------------------------------
optionToString :: Option -> String
optionToString [] = ""
optionToString ((c, st):xr) =
  "[" ++ c ++ " <- " ++ signalTermToString id st ++ "] " ++ optionToString xr

optionWitnessToString :: (Option, [Formula String]) -> String
optionWitnessToString (o, fs) =
  "Hence " ++
  (optionToString o) ++
  " is impossible because of\n" ++
  concatMap (\f -> "    " ++ formulaToString id f ++ "\n") fs ++ "\n"

---------------------------------------------------------------------------
printPredicates :: [(PredicateTerm String, Bool)] -> IO ()
printPredicates xs = do
  putStrLn "The environment chooses:\n"
  _ <-
    sequence $
    fmap
      (\(pt, v) ->
         putStrLn $
         (if v
            then "    "
            else " not") ++
         predicateTermToString id pt)
      xs
  putStrLn ""
  return ()

---------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "TSL Spec:"
  tslPath <- getLine
  putStrLn "AAG Counter Strategy:"
  aagPath <- getLine
  sim <- loadSimulation aagPath tslPath
  runSimulation sim

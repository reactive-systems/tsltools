----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Wonhyuk Choi
--
-- Underapproximates a Temporal Stream Logic Modulo Theories specification
-- into a Temporal Stream Logic specification so that it can be synthesized.
-- Procedure is based on the paper
-- "Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?"
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Control.Monad (liftM2)

import System.Exit (die)

import Config (Configuration(..), Flag(..), parseArguments)

import EncodingUtils (initEncoding)

import FileUtils (writeContent, loadTSLMT, tryReadContent)

import PrintUtils ( Color(..)
                  , ColorIntensity(..)
                  , cPutOutLn
                  )

import TSL ( Error
           , TheoryPredicate
           , Dto
           , Cfg
           , cfgFromSpec
           , predsFromSpec
           , consistencyChecking
           , consistencyDebug
           , solveSat
           , buildDtoList
           , genericError
           , sygusTslAssumption
           )

import Debug.Trace (trace)

-----------------------------------------------------------------------------

tabuateLn :: Int -> String -> String
tabuateLn n = unlines . map (tabulate n) . lines

tabulate :: Int -> String -> String
tabulate n = ((replicate n '\t') ++ )

delWhiteLines :: String -> String
delWhiteLines = unlines . filter (not . null) . lines

writeOutput :: (Show a) => Maybe FilePath -> Either a String -> IO ()
writeOutput _ (Left errMsg)      = die $ show errMsg
writeOutput path (Right content) = writeContent path $ removeDQuote content
  where removeDQuote = filter (/= '\"')

consistency
  :: (String -> ExceptT Error IO Bool) 
  -> Either Error [TheoryPredicate]
  -> IO ()
consistency satSolver preds = do
  result <- runExceptT $ (except preds >>= consistencyDebug satSolver)

  let printTabRed = cPutOutLn Vivid Red . tabuateLn 1
      printConsistencyResult = \case
        Nothing  -> printTabRed "None; predicate is satisfiable."
        Just ass -> printTabRed ass
      printTuple (pred, query, result) = do
        cPutOutLn Vivid Blue "Predicate:"
        putStrLn $ tabuateLn 1 pred
        cPutOutLn Vivid Green "SMT Query:"
        putStrLn $ tabuateLn 1 $ delWhiteLines query
        cPutOutLn Vivid Green "Assumption:"
        printConsistencyResult result
        cPutOutLn Dull Cyan "\n\n----------------------------------------------------\n\n"

  case result of 
    Left  errMsg   -> die $ show errMsg
    Right cResults -> mapM_ printTuple cResults

bind2 :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
bind2 m1 m2 f = do
  x1 <- m1
  x2 <- m2
  f x1 x2

tslmt2tsl
  :: FilePath
  -> ExceptT Error IO Cfg
  -> ExceptT Error IO [TheoryPredicate]
  -> IO ()
tslmt2tsl solverPath cfg preds = (=<<) showResult $ runExceptT $ bind2 cfg preds mkAssumptions
  where 
    mkAssumptions :: Cfg -> [TheoryPredicate] -> ExceptT Error IO [String]
    mkAssumptions cfg = sequence . (map (sygusTslAssumption solverPath cfg)) . buildDtoList

    showResult :: Either Error [String] -> IO ()
    showResult = \case
      Left  err -> cPutOutLn Vivid Red   $ "Error: "   ++ show err ++ "\n"
      Right ass -> mapM_ printSuccess ass
      where printSuccess msg = cPutOutLn Vivid Green $ "Success: " ++ show msg ++ "\n"

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments
  (theory, spec) <- loadTSLMT input
  let toOut     = writeOutput output
      preds     = predsFromSpec theory spec
      cfg       = cfgFromSpec theory spec
      path      = case solverPath of
                    Just solverPath' -> solverPath'
                    Nothing          -> error "Please provide a solver path."
      smtSolver = solveSat path
      exceptTR  = ExceptT . return

  case flag of
    Just Predicates  -> toOut $ fmap (unlines . (map show)) preds
    Just Grammar     -> toOut $ fmap show cfg
    Just Consistency -> consistency smtSolver preds
    Just invalidFlag -> toOut $ genericError $ "Invalid Flag: " ++ show invalidFlag
    Nothing          -> tslmt2tsl path (exceptTR cfg) (exceptTR preds)

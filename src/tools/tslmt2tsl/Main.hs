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
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Data.Either (isRight)

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
           , generateAssumptions
           , generateQueryAssumptionPairs
           )

import Debug.Trace (trace)

-----------------------------------------------------------------------------

bind2 :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
bind2 m1 m2 f = do
  x1 <- m1
  x2 <- m2
  f x1 x2

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

unError :: (Show a) => Either a b -> b
unError = \case
  Left  err -> error $ show err
  Right val -> val 

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

sygus
  :: FilePath
  -> Cfg
  -> [TheoryPredicate]
  -> IO ()
sygus solverPath cfg preds = mapM_ showResult triples
  where 
    dtos    = buildDtoList preds
    pairs   = generateQueryAssumptionPairs solverPath cfg dtos
    triples = zipWith (\dto pair -> (\(a,b) -> (show dto, a, b)) <$> pair) dtos pairs

    showResult :: ExceptT Error IO (String, String, String) -> IO ()
    showResult result = do
      value <- runExceptT result
      case value of 
        Left  err    -> cPutOutLn Vivid Red $ show err
        Right triple -> printSuccess triple

    printSuccess :: (String, String, String) -> IO ()
    printSuccess (dto, query, assumption) = do
      cPutOutLn Vivid Blue "Data Transformation Obligation:"
      putStrLn $ tabuateLn 1 dto
      cPutOutLn Vivid Green "SyGuS Query:"
      putStrLn $ tabuateLn 1 $ delWhiteLines query
      cPutOutLn Vivid Green "Assumption:"
      putStrLn assumption
      cPutOutLn Dull Cyan "\n\n----------------------------------------------------\n\n"

tslmt2tsl
  :: FilePath
  -> String
  -> Either Error Cfg
  -> Either Error [TheoryPredicate]
  -> IO String
tslmt2tsl solverPath tslSpec cfg preds = addAssumptions assumptions
  where 
    assumptions :: Either Error (IO String)
    assumptions = (generateAssumptions solverPath) <$> cfg <*> (buildDtoList <$> preds)

    addAssumptions :: Either Error (IO String) -> IO String
    addAssumptions = \case
      Left  err         -> die $ show err
      Right assumptions -> (tslSpec ++) <$> assumptions

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

  case flag of
    Just Predicates  -> toOut $ fmap (unlines . (map show)) preds
    Just Grammar     -> toOut $ fmap show cfg
    Just Consistency -> consistency smtSolver preds
    Just Sygus       -> sygus path (unError cfg) (unError preds)
    Just invalidFlag -> toOut $ genericError $ "Invalid Flag: " ++ show invalidFlag
    Nothing          -> (tslmt2tsl path "" cfg preds) >>= putStrLn

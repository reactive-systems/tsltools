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

import Data.Maybe (catMaybes)

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
           , Cfg
           , cfgFromSpec
           , predsFromSpec
           , consistencyChecking
           , consistencyDebug
           , solveSat
           , genericError
           , IntermediateResults (..)
           , SygusDebugInfo (..)
           , generateAssumptions
           , buildDtoList
           )

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

unError :: (Show a) => Either a b -> b
unError = \case
  Left  err -> error $ show err
  Right val -> val 

printEnd :: IO ()
printEnd = cPutOutLn Dull Cyan literal
  where literal = "\n\n----------------------------------------------------\n\n"

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
        printEnd

  case result of 
    Left  errMsg   -> die $ show errMsg
    Right cResults -> mapM_ printTuple cResults

printIntermediateResults :: Int -> IntermediateResults -> IO ()
printIntermediateResults numTabs intermediateResults = do
  cPutOutLn Vivid Blue $ tab "Input:"
  putStrLn $ tabMore $ problem intermediateResults
  cPutOutLn Vivid Green $ tab "Query:"
  putStrLn $ tabMore $ query intermediateResults
  cPutOutLn Vivid Green "Result:"
  putStrLn $ tabMore $ result intermediateResults
      where tab     = tabulate numTabs
            tabMore = tabulate (numTabs + 1)

printSygusDebugInfo :: SygusDebugInfo -> IO ()
printSygusDebugInfo = \case
  NextDebug info        -> printIntermediateResults 0 info >> printEnd
  EventuallyDebug infos -> do
    cPutOutLn Vivid Magenta "Recursive Synthesis:"
    mapM_ printPair infos
    printEnd
    where printPair (modelInfo, subqueryInfo) = do
            cPutOutLn Vivid Magenta $ tabulate 1 "Produce Models:"
            printIntermediateResults 2 modelInfo
            cPutOutLn Vivid Magenta $ tabulate 1 "PBE Subquery:"
            printIntermediateResults 2 subqueryInfo

printEither :: (a -> IO ()) -> Either Error a -> IO ()
printEither printer = \case
  Left err  -> cPutOutLn Vivid Red $ show err
  Right val -> printer val

sygus
  :: FilePath
  -> Cfg
  -> [TheoryPredicate]
  -> IO ()
sygus solverPath cfg preds = do
  let debugResults = generateAssumptions solverPath cfg (buildDtoList preds) True
  mapM_ (fmap (printEither printPair) . runExceptT) debugResults

  where printPair (assumption, debugInfo) = do
          case debugInfo of
            Nothing   -> error "Expected DebugInfo, but got NOTHING!!"
            Just info -> printSygusDebugInfo info
          cPutOutLn Vivid Magenta "Assumption: "
          putStrLn assumption

tslmt2tsl
  :: FilePath
  -> String
  -> Cfg
  -> [TheoryPredicate]
  -> IO String
tslmt2tsl solverPath tslSpec cfg preds = (mkAlwaysAssume . (++ tslSpec)) <$> assumptions
  where 
    mkAlwaysAssume assumptions = unlines ["always assume {"
                                         , assumptions
                                         , "}"
                                         ]

    extractAssumption input = do
        either <- runExceptT (fmap fst input)
        return $ case either of 
                   Left  _          -> Nothing
                   Right assumption -> Just assumption

    extractAssumptions = (fmap catMaybes) . sequence . (map extractAssumption)

    resultsList = generateAssumptions solverPath cfg (buildDtoList preds) False
    assumptions = unlines <$> extractAssumptions resultsList

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments
  (theory, spec, specStr) <- loadTSLMT input
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
    Nothing          -> tslmt2tsl path specStr (unError cfg) (unError preds) >> return ()
    Just invalidFlag -> toOut $ genericError $ "Invalid Flag: " ++ show invalidFlag

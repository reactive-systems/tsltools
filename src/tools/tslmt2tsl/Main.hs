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
           , generateConsistencyAssumptions
           , consistencyDebug
           , generateSygusAssumptions
           , sygusDebug
           , IntermediateResults (..)
           , SygusDebugInfo (..)
           , buildDtoList
           )

-----------------------------------------------------------------------------

tabuateLn :: Int -> String -> String
tabuateLn n = unlines . map (tabulate n) . lines

tabulate :: Int -> String -> String
tabulate n = ((replicate n '\t') ++ )

delWhiteLines :: String -> String
delWhiteLines = unlines . filter (not . null) . lines

unError :: (Show a) => Either a b -> b
unError = \case
  Left  err -> error $ show err
  Right val -> val 

printEnd :: IO ()
printEnd = cPutOutLn Dull Cyan literal
  where literal = "\n\n----------------------------------------------------\n\n"

printAssumption :: Int -> String -> IO ()
printAssumption numTabs assumption = do
  cPutOutLn Vivid Magenta $ (tabulate numTabs) "Assumption: "
  putStrLn $ (tabulate (numTabs + 1)) assumption

printIntermediateResults :: Int -> IntermediateResults -> IO ()
printIntermediateResults numTabs intermediateResults = do
  cPutOutLn Vivid Blue $ tab "Input:"
  putStrLn $ tabMore $ problem intermediateResults
  cPutOutLn Vivid Green $ tab "Query:"
  putStrLn $ tabAll $ query intermediateResults
  cPutOutLn Vivid Green $ tab "Result:"
  putStrLn $ tabAll $ result intermediateResults
  printAssumption numTabs $ assumption intermediateResults
    where tab     = tabulate numTabs
          tabMore = tabulate (numTabs + 1)
          tabAll  = unlines . (map tabMore) . lines

printSygusDebugInfo :: SygusDebugInfo -> IO ()
printSygusDebugInfo = \case
  NextDebug info assumption -> do
    cPutOutLn Vivid Magenta "Sequential Program Synthesis: "
    printIntermediateResults 0 info
    printAssumption 0 assumption

  EventuallyDebug infos assumption -> do
    cPutOutLn Vivid Magenta "Recursive Program Synthesis:"
    mapM_ printPair infos
    printAssumption 0 assumption

    where
      printPair (modelInfo, subqueryInfo) = do
        cPutOutLn Vivid Magenta $ tabulate 1 "Produce Models:"
        printIntermediateResults 2 modelInfo
        cPutOutLn Vivid Magenta $ tabulate 1 "PBE Subquery:"
        printIntermediateResults 2 subqueryInfo


printEither :: (Show e) => (a -> IO ()) -> Either e a -> IO ()
printEither printer = \case
  Left err  -> cPutOutLn Vivid Red (show err) >> printEnd 
  Right val -> printer val >> printEnd

printDebug :: (a -> IO ()) -> [ExceptT Error IO a] -> IO ()
printDebug printer = 
  mapM_ ((=<<) (printEither printer) . runExceptT)

consistency :: FilePath -> [TheoryPredicate] -> IO ()
consistency = (printResults .) . consistencyDebug
  where printResults = printDebug (printIntermediateResults 0)

sygus :: FilePath -> Cfg -> [TheoryPredicate] -> IO ()
sygus solverPath cfg preds = printDebug printSygusDebugInfo results
  where results = sygusDebug solverPath cfg (buildDtoList preds)

tslmt2tsl
  :: FilePath
  -> String
  -> Cfg
  -> [TheoryPredicate]
  -> IO String
tslmt2tsl solverPath tslSpec cfg preds = (++ tslSpec) <$> assumptionsBlock
  where 
    mkAlwaysAssume :: String -> String
    mkAlwaysAssume assumptions = unlines ["always assume {"
                                         , assumptions
                                         , "}"
                                         ]
    
    extractAssumption :: (Monad m) => ExceptT e m a -> m (Maybe a)
    extractAssumption result = do
      either <- runExceptT result
      return $ case either of
                 Left  _          -> Nothing
                 Right assumption -> Just assumption

    extractAssumptions :: (Monad m) => [ExceptT e m String] -> m String
    extractAssumptions = (fmap (unlines. catMaybes))
                           . sequence
                           . (map extractAssumption)

    consistencyAssumptions :: IO String
    consistencyAssumptions = extractAssumptions $
                               generateConsistencyAssumptions
                               solverPath
                               preds

    sygusAssumptions :: IO String
    sygusAssumptions = extractAssumptions $
                         generateSygusAssumptions
                         solverPath
                         cfg
                         (buildDtoList preds)

    assumptionsBlock :: IO String
    assumptionsBlock = mkAlwaysAssume <$>
                         ( (++) <$>
                           consistencyAssumptions <*>
                           sygusAssumptions
                         )

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments

  (theory, spec, specStr) <- loadTSLMT input

  let writeOut  = writeContent output . (filter (/= '\"'))
      preds     = unError $ predsFromSpec theory spec
      cfg       = unError $ cfgFromSpec theory spec
      path      = case solverPath of
                    Just solverPath' -> solverPath'
                    Nothing          -> error "Please provide a solver path."

  case flag of
    Just Predicates  -> writeOut $ unlines $ map show $ preds
    Just Grammar     -> writeOut $ show cfg
    Just Consistency -> consistency path preds
    Just Sygus       -> sygus path cfg preds
    Nothing          -> writeOut =<< tslmt2tsl path specStr cfg preds
    Just invalidFlag -> die $ "Invalid Flag: " ++ show invalidFlag

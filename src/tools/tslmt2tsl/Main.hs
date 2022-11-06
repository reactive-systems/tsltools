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

printIntermediateResults :: Int -> IntermediateResults -> IO ()
printIntermediateResults numTabs intermediateResults = do
  cPutOutLn Vivid Blue $ tab "Input:"
  putStrLn $ tabMore $ problem intermediateResults
  cPutOutLn Vivid Green $ tab "Query:"
  putStrLn $ tabMore $ query intermediateResults
  cPutOutLn Vivid Green "Result:"
  putStrLn $ tabMore $ result intermediateResults
  cPutOutLn Vivid Magenta "Assumption: "
  putStrLn $ tabMore $ assumption intermediateResults
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

printEither :: (Show e) => (a -> IO ()) -> Either e a -> IO ()
printEither printer = \case
  Left err  -> cPutOutLn Vivid Red $ show err
  Right val -> printer val

printDebug :: (Show e) => (a -> IO ()) -> [ExceptT e IO a] -> IO ()
printDebug printer = 
  mapM_ (fmap (printEither printer) . runExceptT)

consistency :: FilePath -> [TheoryPredicate] -> IO ()
consistency = (printResult .) . consistencyDebug
  where printResult = printDebug (printIntermediateResults 1)

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

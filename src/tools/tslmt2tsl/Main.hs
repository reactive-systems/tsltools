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

import System.Exit(die)

import Config (Configuration(..), Flag(..), parseArguments)

import EncodingUtils (initEncoding)

import FileUtils (writeContent, loadTSLMT, loadTSLMTRaw)

import PrintUtils ( Color(..)
                  , ColorIntensity(..)
                  , cPutOutLn
                  )

import TSL ( SymbolTable(..)
           , Error
           , TheoryPredicate
           , cfgFromSpec
           , predsFromSpec
           , consistencyChecking
           , consistencyDebug
           , solveSat
           , genericError
           , fromTSL
           )

-----------------------------------------------------------------------------

tabuateLn :: Int -> String -> String
tabuateLn n = unlines . map (tabulate n) . lines

tabulate :: Int -> String -> String
tabulate n = ((replicate n '\t') ++ )

delWhiteLines :: String -> String
delWhiteLines = unlines . filter (not . null) . lines

-- Using TSL.Specification occurs extra overhead,
-- so resorting to strings instead.
tslmt2tsl :: String -> [String] -> String
tslmt2tsl tslmtSpec assumptions = assumptions' ++ tslmtSpec
    where header       = "assume {\n"
          tailer       = "\n}\n"
          tabulated    = map (tabulate 1) assumptions
          assumptions' = header ++ unlines tabulated ++ tailer

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

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments

  (theory, specStr) <- loadTSLMTRaw input
  (_, spec)         <- loadTSLMT input -- XXX
  
  let satSolver = solveSat solverPath
      preds     = predsFromSpec theory spec
      toOut     = writeOutput output

  case flag of
    (Just Predicates)  -> toOut $ fmap (unlines . (map show)) preds
    (Just Grammar)     -> toOut $ Right $ show $ cfgFromSpec spec
    (Just Consistency) -> consistency satSolver preds
    (Just flag')       -> toOut $ genericError $ "Unimplemented flag: " ++ show flag'
    Nothing            -> do -- end-to-end is currently just consistency checking.
      consistency <- runExceptT $ (except preds >>= consistencyChecking satSolver)
      let tslWithAssumptions = (tslmt2tsl specStr) <$> consistency
      toOut tslWithAssumptions

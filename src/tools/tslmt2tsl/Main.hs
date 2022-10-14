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
           , buildDto
           , fixedSizeQuery
           , genericError
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

sygusDebug :: [TheoryPredicate] -> Cfg -> String
sygusDebug predicates = fixedSizeQuery dto
  where pred = head predicates
        dto  = buildDto pred pred 

-- preds :: Either Error [TheoryPredicate]
-- cfgFromSpec :: Theory -> Specification -> Either Error Cfg
--

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments

  case flag of
    Nothing            -> error "No flag option not yet supported; please provide a flag."
    (Just flag')       -> do
      (theory, spec) <- loadTSLMT input
      let toOut              = writeOutput output
      let preds              = predsFromSpec theory spec
          smtSolver          = case solverPath of
                                 Just path -> solveSat path
                                 Nothing   -> error "Please provide a solver path."
      case flag' of 
        Predicates  -> toOut $ fmap (unlines . (map show)) preds
        Grammar     -> toOut $ fmap show $ cfgFromSpec theory spec
        Consistency -> consistency smtSolver preds
        Sygus       -> toOut $ liftM2 sygusDebug preds $ cfgFromSpec theory spec
        invalidFlag -> toOut $ genericError $ "Invalid Flag: " ++ show invalidFlag

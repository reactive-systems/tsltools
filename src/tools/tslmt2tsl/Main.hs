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

import FileUtils (writeContent, loadTSLMT)

import TSL ( Specification(..)
           , SymbolTable(..)
           , Error
           , cfgFromSpec
           , predsFromSpec
           , consistencyChecking
           , solveSat
           , genericError
           , debug
           )

-----------------------------------------------------------------------------

writeOutput :: (Show a) => Maybe FilePath -> Either a String -> IO ()
writeOutput _ (Left errMsg)      = die $ show errMsg
writeOutput path (Right content) = writeContent path $ removeDQuote content
  where removeDQuote = filter (/= '\"')

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag, solverPath} <- parseArguments

  (theory, spec) <- loadTSLMT input
  
  let satSolver = solveSat solverPath -- String -> ExceptT Error IO Bool
      preds     = predsFromSpec theory spec
      toOut     = writeOutput output

  case flag of
    (Just Predicates)  -> toOut $ fmap (unlines . (map show)) preds
    (Just Grammar)     -> toOut $ Right $ show $ cfgFromSpec spec
    (Just Consistency) -> toOut =<< (runExceptT $ fmap unlines $ except preds >>= (consistencyChecking satSolver))
    (Just flag')       -> toOut $ genericError $ "Unimplemented flag: " ++ show flag'
    Nothing            -> toOut $ genericError $ "tslmt2tsl end-to-end not yet supported"

-- preds :: Either Error TheoryPredicate
-- input :: ExceptT Error IO TheoryPredicate
-- consistencyChecking satSolver :: ExceptT IO Error [String]

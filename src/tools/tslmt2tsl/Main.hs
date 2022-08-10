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
           -- , applySemantics
           )

import System.Exit(die)

-----------------------------------------------------------------------------

writeOutput :: (Show a) => Maybe FilePath -> Either a String -> IO ()
writeOutput _ (Left errMsg)      = die $ show errMsg
writeOutput path (Right content) = writeContent path $ removeDQuote content
  where removeDQuote = filter (/= '\"')

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag} <- parseArguments

  (theory, spec) <- loadTSLMT input
  
  let preds   = predsFromSpec theory spec
      content = case flag of
        (Just Predicates)  -> fmap (unlines . (map show)) preds
        (Just Grammar)     -> Right $ show $ cfgFromSpec spec
        -- (Just Consistency) -> unlines <$> (preds >>= consistencyChecking)
        (Just flag')       -> genericError $ "Unimplemented flag: " ++ show flag'
        Nothing            -> genericError $ "tslmt2tsl end-to-end not yet supported"

  writeOutput output content

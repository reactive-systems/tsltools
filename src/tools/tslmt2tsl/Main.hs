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

import FileUtils (writeContent, loadTSL)

import TSL (fromSpec, getPredicateTerms)

import System.Exit(die)

-----------------------------------------------------------------------------

writeOutput :: Maybe FilePath -> Either String String -> IO ()
writeOutput _ (Left errMsg)      = die errMsg
writeOutput path (Right content) = writeContent path content

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag} <- parseArguments

  spec <- loadTSL input
  
  let content = case flag of
                  (Just Predicates) -> Right $ show $ getPredicateTerms spec
                  (Just Grammar)    -> Right $ show $ fromSpec spec
                  Nothing           -> Left $ "tslmt2tsl end-to-end not yet supported"
                  (Just flag')      -> Left $ "Unimplemented flag: " ++ show flag'

  writeOutput output content

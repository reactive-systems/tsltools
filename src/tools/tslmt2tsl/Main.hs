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
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config (Configuration(..), Flag(..), parseArguments)

import EncodingUtils (initEncoding)

import FileUtils (writeContent, loadTSL)

import TSL (CFG(..), fromSpec)

import System.Exit(die)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag} <- parseArguments
  spec <- loadTSL input
  case flag of
      (Just Grammar) -> writeContent output (show $ fromSpec spec)
      _              -> die $ "Unsupported Flag" ++ show flag

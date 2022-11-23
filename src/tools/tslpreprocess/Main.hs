----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Wonhyuk Choi
--
-- Underapproximates a Temporal Stream Logic Modulo Theories specification
-- into a Temporal Stream Logic specification so that it can be synthesized.
-- Procedure is based on the paper
-- "Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?"
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import Config (Configuration (..), parseArguments)
import EncodingUtils (initEncoding)
import FileUtils (tryReadContent, writeContent)
import System.Exit (die)
import TSL (preprocess)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  initEncoding
  Configuration {input, output} <- parseArguments
  content <- tryReadContent input
  case preprocess content of
    Left errMsg -> die $ show errMsg
    Right processed -> writeContent output $ show processed

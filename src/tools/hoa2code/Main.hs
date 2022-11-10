----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
--
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config (Configuration(..), parseArguments)

import EncodingUtils (initEncoding)


import TSL (implementHoa)

import Hanoi (parse)
import Data.Maybe (fromJust)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, codeTarget} <- parseArguments
  c <- readFile $ fromJust input
  let hoa = parse c
  putStrLn $ either id (implementHoa codeTarget) hoa
  --cfm <- loadCFM input

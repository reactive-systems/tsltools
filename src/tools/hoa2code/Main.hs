----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import Config (Configuration (..), parseArguments)
import Data.Maybe (fromJust)
import EncodingUtils (initEncoding)
import Hanoi (parse)
import TSL (implementHoa)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  Configuration {input, codeTarget} <- parseArguments
  c <- readFile $ fromJust input
  let hoa = parse c
  putStrLn $ either id (implementHoa codeTarget) hoa

-- cfm <- loadCFM input

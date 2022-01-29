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


import TSL ( decodeOutputAP, decodeInputAP, tslFormula, implementHoa )
import qualified TSL as T (Formula(..))
import Data.List ( isPrefixOf, isInfixOf )
import Data.Tuple ( swap )

import Hanoi
    ( HOA(..), parse, AcceptanceSet, Label, State, Formula(..) )
import Data.Maybe ( fromJust, maybeToList )
import Data.List as List (intercalate, sortOn)

import Finite (Finite, FiniteBounds, index, offset, v2t, values)

import qualified Data.Map as M
import Data.Set as Set (Set, elems, toList)
import qualified Data.Bifunctor

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, output, codeTarget, moduleName, functionName} <- parseArguments
  c <- readFile $ fromJust input
  let hoa = parse c
  putStrLn $ either id (implementHoa codeTarget) hoa
  --cfm <- loadCFM input

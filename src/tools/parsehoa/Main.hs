----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------


import EncodingUtils (initEncoding)

import TSL (implement)
import TSL (decodeOutputAP) 
import TSL (decodeInputAP)
import TSL (tslFormula)
import qualified TSL as T (Formula(..))
import Data.Either 
import Data.Char 
import Data.List 
import Data.Tuple

import Hanoi 
  ( HOA(..)
  , parse
  )
import System.Environment
import Data.Maybe
import Data.Tuple
import Data.List as List (intercalate, sortOn)

import Finite (Finite, FiniteBounds, index, offset, v2t, values)

import qualified Data.Map as M
import Data.List as List (intercalate, sortOn)
import Data.Maybe (maybeToList)
import Data.Set as Set (Set, elems, toList)
import Finite (Finite, FiniteBounds, index, offset, v2t, values)
import Hanoi
  ( AcceptanceSet
  , AcceptanceType(..)
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
  , State
  , printHOA
  )
import Hanoi (Formula(..))

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding
  args <- getArgs
  c <- readFile $ head args
  let hoa = parse c
  putStrLn $ either id (printHOA. translateEdges) hoa

translateEdges :: HOA -> HOA
translateEdges hoa@HOA {..} =
  hoa {atomicPropositionName = translateToTSL . atomicPropositionName} 

translateToTSL :: String -> String
translateToTSL t = 
  if isPrefixOf "p0" t
  then generateTSLString T.Check decodeInputAP t
  else generateTSLString (uncurry T.Update) decodeOutputAP t

generateTSLString :: forall a b. _ -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either (const "ERR") (\t -> (tslFormula id $ tslType t)) $
    (decoder) x

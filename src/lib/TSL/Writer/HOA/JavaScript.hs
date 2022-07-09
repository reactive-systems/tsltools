----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Danielle Cai, Rhea Kotari
--
-- Generates javascript code from a HOA file generated from a TSL spec
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}


module TSL.Writer.HOA.JavaScript
  ( implementHoa
  ) where

import TSL.Writer.HOA.Utils

import Data.Tuple ( swap )

import qualified Data.Text as T

import Hanoi
    ( HOA(..), AcceptanceSet, Label, State )
import Data.Maybe ( fromJust )
import Data.List as List (intercalate, sortOn)

import Finite (FiniteBounds, values)

import qualified Data.Map as M
import Data.Set as Set (Set, toList)
import qualified Data.Bifunctor

implementHoa :: HOA -> String
implementHoa = concat . printHOALines

printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let
    -- TODO pretty sure this doesnt need to be sorted
    apNamesSorted = map (Data.Bifunctor.first atomicPropositionName) $ sortOn (atomicPropositionName. fst) $ zip values [0..]
    apNamesMap = M.fromList $ map swap apNamesSorted

    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      unwords (
        ["if (currentState == "]
        ++
        [strInd s]
        ++
        ["){"]
      )
      :
      map printEdge (toList $ edges s) ++ ["}"]

    printEdge ::
          FiniteBounds HOA
      => ([State], Maybe Label, Maybe (Set AcceptanceSet))
      -> String
    printEdge edge =
      let
        (target, label, _) = edge
        -- we should only ever had one target state in TSL models (I think), so `head` works
        stateUpdate = "currentState = " ++ strInd (head target) ++ ";"
      in
        printLabel (fromJust label) stateUpdate

    printLabel :: FiniteBounds HOA => Label -> String -> String
    printLabel label stateUpdate = let

        splitFormulas = formulaToList label
        termStringList = map (map (printTSLFormula negationOperator (strIndWithMap apNamesMap))) splitFormulas :: [[String]]
        predUpds = splitPredUpdates negationOperator termStringList
        predUpdToCode (preds, upds) = let
            conditional =  if null preds then "true" else List.intercalate (" "++conjunctionOperator++ indent 3) preds
            body = indent 4 ++ List.intercalate (indent 4) (map updateToAssignment upds ++ [stateUpdate])
          in
            "if (" ++ conditional ++ "){" ++ body ++ indent 2 ++ "}" ++ "\n"
      in
        concatMap (\x -> indent 2 ++ predUpdToCode x) predUpds
  in
    List.intercalate ["\nelse "] $ map printState values

-----------------------------------------------------------------------------
-- | Language specific functions

updateToAssignment :: String -> String
updateToAssignment x = let
  constsSaved = T.unpack $ T.replace "()" "cccccc" $ T.pack (traceShowId x)
  noBrackets = filter (\c -> not $ c `elem` ['[',']','(',')']) constsSaved
  [val, assignment] = T.splitOn " <- " $ T.pack noBrackets
  fxnParts = map T.strip $ T.splitOn " " assignment
  params = if tail fxnParts == []
           then ""
           else T.replace "cccccc" "()" $ T.concat ["(", (T.intercalate ", " $ tail fxnParts), ");"] 
 in
   T.unpack $ T.concat [val, " = ", head fxnParts, params]
   
negationOperator :: String
negationOperator = "!"

conjunctionOperator :: String
conjunctionOperator = "&&"


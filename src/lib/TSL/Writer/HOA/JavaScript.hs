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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}


module TSL.Writer.HOA.JavaScript
  ( implementHoa
  ) where

import TSL.Writer.HOA.Utils

import qualified TSL.Logic as T (Formula(..), decodeOutputAP, decodeInputAP, tslFormula )
import Data.List ( isPrefixOf, isInfixOf )
import Data.Tuple ( swap )

import Data.Text(pack, unpack, replace, Text)

import Hanoi
    ( HOA(..), AcceptanceSet, Label, State, Formula(..) )
import Data.Maybe ( fromJust )
import Data.List as List (intercalate, sortOn)

import Finite (Finite, FiniteBounds, index, offset, v2t, values)

import qualified Data.Map as M
import Data.Set as Set (Set, toList)
import qualified Data.Bifunctor

implementHoa :: HOA -> String
implementHoa = unlines . printHOALines

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
        ["\nif (currentState == "]
        ++
        [strInd s]
        ++
        ["){"]
      )
      :
      map printEdge (toList $ edges s)

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
        termStringList = map (map (printTSLFormula (strIndWithMap apNamesMap))) splitFormulas :: [[String]]
        predUpds = splitPredUpdates termStringList
        predUpdToCode (preds, upds) = let
            conditional =  if preds == [] then "true" else intercalate (" &&"++ indent 3) preds
            body = indent 4 ++ intercalate (indent 4) (map updateToAssignment upds ++ [stateUpdate]) ++ "\n}"
          in
            "if (" ++ conditional ++ "){" ++ body ++ "\n}"
      in
        concatMap (\x -> indent 2 ++ predUpdToCode x) predUpds
  in
    concatMap printState values

-----------------------------------------------------------------------------
-- | Language specific functions

updateToAssignment :: String -> String
updateToAssignment =
  filter (\c -> c /= '[' && c /= ']'). (replaceUpdate (++ myStringInput))

replaceUpdate :: (String -> String) -> String -> String
replaceUpdate myStringInput = unpack . replace "<-" assignmentOperator . pack

myStringInput :: String
myStringInput = ";"

negationOperator :: String
negationOperator = "!"

assignmentOperator :: Text
assignmentOperator = "="

-----------------------------------------------------------------------------
-- | Language specific functions


strIndWithMap :: (Finite HOA a, FiniteBounds HOA) => M.Map Int String -> a -> String
strIndWithMap nameMap a = translateToTSL $ nameMap M.! (index a - offset (v2t a))

strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = show (index a - offset (v2t a))

translateToTSL :: String -> String
translateToTSL t =
  if "p0" `isPrefixOf` t
  then generateTSLString T.Check T.decodeInputAP t
  else generateTSLString (uncurry T.Update) T.decodeOutputAP t

generateTSLString :: forall a b. Show a => (b -> T.Formula String) -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either show (T.tslFormula id. tslType) $
    decoder x

-- | Given a list of 
splitPredUpdates :: [[String]] -> [([String], [String])]
splitPredUpdates terms = let
    sortTerm t (preds, upds) =
      if "<-" `isInfixOf` t
      then if negationOperator `isPrefixOf` t
        then (preds, upds)
        else (preds, t:upds)
      else (t:preds, upds)
  in
    map (foldr sortTerm ([],[])) terms

-- | in TSL, the formulas on the edges will always be conjunctions separated by disjunctions
--   so we just pull the formula apart, then can sort by preds and updates 
formulaToList :: Formula a -> [[Formula a]]
formulaToList f =
  let
    conjunctionsToList :: Formula a -> [Formula a]
    conjunctionsToList f = case f of
      FAnd fs -> concatMap conjunctionsToList fs
      _ -> [f]

    disjunctionsToList :: Formula a -> [Formula a]
    disjunctionsToList f = case f of
      FOr fs -> concatMap disjunctionsToList fs
      _ -> [f]
  in
    map conjunctionsToList $ disjunctionsToList f

-- | When printing a TSL formula, we should only have the vars or negated vars
--   if we have anything else, we did not have only conjunctions and need to revisit assumption of formulaToList
printTSLFormula :: (a -> String) -> Formula a -> String
printTSLFormula showVar = \case
  FVar a -> showVar a
  FNot f -> negationOperator ++ printSubFormula f
  _ -> error "unexpected formula structure on transition"

  where
    printSubFormula = brRound . printTSLFormula showVar


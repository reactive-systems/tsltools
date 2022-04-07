----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates an XState from a HOA file generated from a TSL spec
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
{-# LANGUAGE OverloadedStrings #-}


module TSL.Writer.HOA.XState
  ( implementHoa
  ) where

import qualified TSL.Logic as T (Formula(..), decodeOutputAP, decodeInputAP, tslFormula ) 
import Data.List ( isPrefixOf, isInfixOf )
import Data.Tuple ( swap )

import Data.Text(pack, unpack, replace)

import Hanoi
    ( HOA(..), parse, AcceptanceSet, Label, State, Formula(..) )
import Data.Maybe ( fromJust, maybeToList )
import Data.List as List (intercalate, sortOn)

import Finite (Finite, FiniteBounds, index, offset, v2t, values)

import qualified Data.Map as M
import Data.Set as Set (Set, elems, toList)
import qualified Data.Bifunctor

implementHoa :: HOA -> String
implementHoa = (unlines . printHOALines)

printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let
    apNamesSorted = map (Data.Bifunctor.first atomicPropositionName) $ sortOn (atomicPropositionName. fst) $ zip values [0..]
    apNamesMap = M.fromList $ map swap apNamesSorted

    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      unwords (
        [strInd s]
        ++
        [": {"]
        ++
        ["\n on: {"]
      )
      :
      (map printEdge (zip (toList $ edges (s)) [1..])) ++ ["}"] ++ ["},"]

    printEdge ::
          FiniteBounds HOA
      => (([State], Maybe Label, Maybe (Set AcceptanceSet)), Integer)
      -> String
    printEdge edge =
      let
        ((target, label, _), nlab) = edge
        stateUpdate = "target: \'" ++ printStateConj target ++ "\',\n},"
      in
        printLabel (fromJust label) stateUpdate nlab

    printLabel :: FiniteBounds HOA => Label -> String -> Integer -> String
    printLabel label stateUpdate n = let

        splitFormulas = formulaToList label
        termStringList = map (map (printTSLFormula strInd2)) splitFormulas :: [[String]]
        predUpds = splitPredUpdates termStringList
        predUpdToCode (predsupds, num) = let
            (preds,upds)=predsupds
            conditional =  if preds == [] then "True" else intercalate (" and" ++ indent 3) preds
            body = indent 4 ++ intercalate (indent 4) (((["actions: ["] ++ map (wrap "\'" "\',") (map updateToAssignment (upds )) ++ ["],"]) ++ [stateUpdate]))
          in
             "t" ++ show n ++ show num  ++ ":\n {description: \'" ++ conditional ++ "\'," ++ body
      in
      --add zip below
        concatMap (\x -> indent 2 ++ predUpdToCode x) (zip predUpds [1..])

    printStateConj :: FiniteBounds HOA => [State] -> String
    printStateConj = intercalate " & " . map strInd

    strInd2 = strIndWithMap apNamesMap
  in
     ["states: {"] ++ concatMap printState values ++ ["}"]

-----------------------------------------------------------------------------
-- | Different library related printing methods

updateToAssignment :: String -> String
updateToAssignment = 
  filter (\c -> c /= '[' && c /= ']'). replaceUpdate

replaceUpdate :: String -> String
replaceUpdate = unpack . replace "<-" "=" . pack

actionMake :: String -> String
actionMake = 
  filter (\c -> c /= '[' && c /= ']'). replaceUpdate

negationSymbol :: String
negationSymbol = "not"

indent :: Int -> String
indent n = let
    indentLevel = "  "
  in
    "\n" ++ concat (replicate n indentLevel)
strIndWithMap :: (Finite HOA a, FiniteBounds HOA) => M.Map Int String -> a -> String
strIndWithMap nameMap a = translateToTSL $ nameMap M.! (index a - offset (v2t a))

strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = show (index a - offset (v2t a))

wrap :: String -> String -> String -> String
wrap prefix suffix s = prefix ++ s ++ suffix

brRound :: String -> String
brRound = wrap "(" ")"

translateToTSL :: String -> String
translateToTSL t =
  if isPrefixOf "p0" t
  then generateTSLString T.Check T.decodeInputAP t
  else generateTSLString (uncurry T.Update) T.decodeOutputAP t

-- TODO need to specialize this to Python syntax, rather than just using TSL syntax
-- for pred terms just add parens, for update terms, a little bit more parsing needed
generateTSLString :: forall a b. Show a => (b -> T.Formula String) -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either show (T.tslFormula id. tslType) $
    decoder x

splitPredUpdates :: [[String]] -> [([String], [String])]
splitPredUpdates terms = let
    sortTerm t (preds, upds) =
      if isInfixOf "<-" t
      then if isPrefixOf negationSymbol t
        then (preds, upds)
        else (preds, t:upds)
      else (t:preds, upds)
  in
    map (foldr sortTerm ([],[])) terms

-- | in TSL, the formulas on the edges will always be conjunctions separated by disjunctions
--   so we just pull the formula apart, then can sort by preds and updates 
formulaToList :: Formula a -> [[Formula a]]
formulaToList f =
  map conjunctionsToList $ disjunctionsToList f

conjunctionsToList :: Formula a -> [Formula a]
conjunctionsToList f = case f of
  FAnd fs -> concatMap conjunctionsToList fs
  _ -> [f]

disjunctionsToList :: Formula a -> [Formula a]
disjunctionsToList f = case f of
  FOr fs -> concatMap disjunctionsToList fs
  _ -> [f]

-- | When printing a TSL formula, we should only have the vars or negated vars
--   if we have anything else, we did not have only conjunctions and need to revisit assumption of formulaToList
printTSLFormula :: (a -> String) -> Formula a -> String
printTSLFormula showVar = \case
  FVar a -> showVar a
  FNot f -> negationSymbol ++ printSubFormula f
  _ -> error "unexpected formula structure on transition"

  where
    printSubFormula = brRound . printTSLFormula showVar


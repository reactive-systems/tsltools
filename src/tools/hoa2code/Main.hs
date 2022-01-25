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

import Config (Configuration(..), parseArguments)

import EncodingUtils (initEncoding)


import TSL ( decodeOutputAP, decodeInputAP, tslFormula )
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
  putStrLn $ either id printHOA hoa
  --cfm <- loadCFM input

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' to as a 'String'

printHOA :: HOA -> String
printHOA = unlines . printHOALines

-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' as a list of 'String's representing
-- different lines

printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let
    apNamesSorted = map (Data.Bifunctor.first atomicPropositionName) $ sortOn (atomicPropositionName. fst) $ zip values [0..]
    apNamesMap = M.fromList $ map swap apNamesSorted

    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      unwords (
        ["\nif (currentState == "]
        ++
        [strInd s]
        ++
        ["):", indent 1]
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
        stateUpdate = "currentState = " ++ printStateConj target
      in
        printLabel (fromJust label) stateUpdate

    printLabel :: FiniteBounds HOA => Label -> String -> String
    printLabel label stateUpdate = let

        splitFormulas = formulaToList label
        termStringList = map (map (printTSLFormula strInd2)) splitFormulas :: [[String]]
        predUpds = splitPredUpdates termStringList
        predUpdToCode (preds, upds) = let
            conditional =  intercalate (" &&" ++ indent 3) preds
            body = indent 4 ++ intercalate (indent 4) (upds ++ [stateUpdate])
          in
            "if (" ++ conditional ++ "):" ++ body
      in
        concatMap (\x -> indent 2 ++ predUpdToCode x) predUpds

    printStateConj :: FiniteBounds HOA => [State] -> String
    printStateConj = intercalate " & " . map strInd

    strInd2 = strIndWithMap apNamesMap
  in
    concatMap printState values

-----------------------------------------------------------------------------
-- | Different library related printing methods

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
  then generateTSLString T.Check decodeInputAP t
  else generateTSLString (uncurry T.Update) decodeOutputAP t

generateTSLString :: forall a b. Show a => _ -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either show (tslFormula id. tslType) $
    decoder x


splitPredUpdates :: [[String]] -> [([String], [String])]
splitPredUpdates terms = let
    sortTerm t (preds, upds) =
      if isInfixOf "<-" t
      then if isPrefixOf "!" t
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
  FNot f -> "!" ++ printSubFormula f
  _ -> error "unexpected formula structure on transition"

  where
    printSubFormula = brRound . printTSLFormula showVar


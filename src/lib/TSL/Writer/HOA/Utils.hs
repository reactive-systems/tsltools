{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module TSL.Writer.HOA.Utils
  ( brRound
  , indent
  , strInd
  , strIndWithMap
  , translateToTSL
  , formulaToList
  , splitPredUpdates
  , printTSLFormula
  ) where
import Finite ( v2t, Finite(offset, index), FiniteBounds )
import Hanoi (HOA, Formula (FAnd, FOr, FVar, FNot))
import qualified Data.Map as M
import qualified TSL.Logic as T
import Data.List ( isPrefixOf, isInfixOf )

wrap :: String -> String -> String -> String
wrap prefix suffix s = prefix ++ s ++ suffix

brRound :: String -> String
brRound = wrap "(" ")"

indent :: Int -> String
indent n = let
    indentLevel = "  "
  in
    "\n" ++ concat (replicate n indentLevel)
  

strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = show (index a - offset (v2t a))
  
strIndWithMap :: (Finite HOA a, FiniteBounds HOA) => M.Map Int String -> a -> String
strIndWithMap nameMap a = translateToTSL $ nameMap M.! (index a - offset (v2t a))

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
splitPredUpdates :: String -> [[String]] -> [([String], [String])]
splitPredUpdates negationOperator terms = let
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
printTSLFormula :: String -> (a -> String) -> Formula a -> String
printTSLFormula negationOperator showVar = \case
  FVar a -> showVar a
  FNot f -> negationOperator ++ printSubFormula f
  _ -> error "unexpected formula structure on transition"

  where
    printSubFormula = brRound . printTSLFormula negationOperator showVar
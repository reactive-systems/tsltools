-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen.CoreGen
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Transform specification and formulas to strings 
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.ToString
  ( tslSpecToString
  , specToString
  ) where

import TSL.Specification (Specification(..), TSLSpecification(..))

import TSL.Logic
  ( Formula(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  )

import TSL.SymbolTable (SymbolTable, stName)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
insertInside :: [a] -> a -> [a]
insertInside [] _ = []
insertInside [x] _ = [x]
insertInside (x:xr) a = x : a : insertInside xr a

signalTermToString :: SymbolTable -> SignalTerm Int -> String
signalTermToString sym =
  \case
    Signal a -> stName sym a
    FunctionTerm ft -> functionTermToString sym ft
    PredicateTerm pt -> predicateTermToString sym pt

functionTermToString :: SymbolTable -> FunctionTerm Int -> String
functionTermToString sym =
  \case
    FunctionSymbol a -> stName sym a
    FApplied ft st ->
      "(" ++
      functionTermToString sym ft ++ " " ++ signalTermToString sym st ++ ")"

predicateTermToString :: SymbolTable -> PredicateTerm Int -> String
predicateTermToString sym =
  \case
    BooleanTrue -> "true"
    BooleanFalse -> "false"
    BooleanInput a -> stName sym a
    PredicateSymbol a -> stName sym a
    PApplied pt st ->
      "(" ++
      predicateTermToString sym pt ++ " " ++ signalTermToString sym st ++ ")"

formulaToString :: SymbolTable -> (Formula Int) -> String
formulaToString sym =
  \case
    TTrue -> "true"
    FFalse -> "false"
    Check pt -> predicateTermToString sym pt
    Update c st ->
      "[" ++ stName sym c ++ " <- " ++ signalTermToString sym st ++ "]"
    Not f -> "(! " ++ formulaToString sym f ++ ")"
    Implies f g ->
      "(" ++ formulaToString sym f ++ " -> " ++ formulaToString sym g ++ ")"
    Equiv f g ->
      "(" ++ formulaToString sym f ++ " <-> " ++ formulaToString sym g ++ ")"
    And xs ->
      "(" ++ concat (insertInside (map (formulaToString sym) xs) " && ") ++ ")"
    Or xs ->
      "(" ++ concat (insertInside (map (formulaToString sym) xs) " || ") ++ ")"
    Next f -> "(X " ++ formulaToString sym f ++ ")"
    Globally f -> "(G " ++ formulaToString sym f ++ ")"
    Finally f -> "(F " ++ formulaToString sym f ++ ")"
    Until f g ->
      "(" ++ formulaToString sym f ++ " U " ++ formulaToString sym g ++ ")"
    Release f g ->
      "(" ++ formulaToString sym f ++ " R " ++ formulaToString sym g ++ ")"
    Weak f g ->
      "(" ++ formulaToString sym f ++ " W " ++ formulaToString sym g ++ ")"

-----------------------------------------------------------------------------
specToString :: Specification -> String
specToString Specification {..} =
  "initially guarantee {" ++ formulaToString symboltable formula ++ "}"

tslSpecToString :: TSLSpecification -> String
tslSpecToString TSLSpecification {..} =
  "initially assume {\n" ++
  help assumptions ++ "}\n\ninitially guarantee {\n" ++ help guarantees ++ "}"
  where
    help :: [Formula Int] -> String
    help xs =
      concatMap (\f -> "  " ++ formulaToString tslSymboltable f ++ ";\n") xs

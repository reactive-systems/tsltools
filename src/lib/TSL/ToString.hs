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

import TSL.Specification
  ( Specification(..)
  , TSLSpecification(..)
  , tslSpecToSpec
  )

import TSL.Logic (Formula(..))

import TSL.SymbolTable (SymbolTable)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
insertInside :: [a] -> a -> [a]
insertInside [] a = []
insertInside [x] a = [a]
insertInside (x:xr) a = x : a : insertInside xr a

formulaToString sym =
  \case
    TTrue -> "true"
    FFalse -> "false"
    Check pt -> "TODO"
    Update c st -> "TODO"
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
      concatMap (\f -> "  " ++ formulaToString symboltable f ++ ";\n") xs

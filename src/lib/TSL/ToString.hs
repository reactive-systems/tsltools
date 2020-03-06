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
  , formulaToString
  , predicateTermToString
  , signalTermToString
  ) where

-----------------------------------------------------------------------------
import TSL.Specification (Specification(..), TSLSpecification(..))

import TSL.Logic
  ( Formula(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  )

import TSL.SymbolTable (stName)

-----------------------------------------------------------------------------
insertInside :: [a] -> a -> [a]
insertInside [] _ = []
insertInside [x] _ = [x]
insertInside (x:xr) a = x : a : insertInside xr a

-----------------------------------------------------------------------------
-- | Given a symbol naming function, convert a signal term to a string
signalTermToString :: (a -> String) -> SignalTerm a -> String
signalTermToString env =
  \case
    Signal a -> env a
    FunctionTerm ft ->
      case ft of
        FunctionSymbol a -> env a ++ "()"
        ft -> functionTermToString env ft
    PredicateTerm pt -> predicateTermToString env pt

-----------------------------------------------------------------------------
-- | Given a symbol naming function, convert a function term to a string
functionTermToString :: (a -> String) -> FunctionTerm a -> String
functionTermToString env =
  \case
    FunctionSymbol a -> env a
    FApplied ft st ->
      "(" ++
      functionTermToString env ft ++ " " ++ signalTermToString env st ++ ")"

-----------------------------------------------------------------------------
-- | Given a symbol naming function, convert a predicate term to a string
predicateTermToString :: (a -> String) -> PredicateTerm a -> String
predicateTermToString env =
  \case
    BooleanTrue -> "true"
    BooleanFalse -> "false"
    BooleanInput a -> env a
    PredicateSymbol a -> env a
    PApplied pt st ->
      "(" ++
      predicateTermToString env pt ++ " " ++ signalTermToString env st ++ ")"

-----------------------------------------------------------------------------
-- | Given a symbol naming function, convert a formula to a string
formulaToString :: (a -> String) -> (Formula a) -> String
formulaToString env =
  \case
    TTrue -> "true"
    FFalse -> "false"
    Check pt -> predicateTermToString env pt
    Update c st -> "[" ++ env c ++ " <- " ++ signalTermToString env st ++ "]"
    Not f -> "(! " ++ formulaToString env f ++ ")"
    Implies f g ->
      "(" ++ formulaToString env f ++ " -> " ++ formulaToString env g ++ ")"
    Equiv f g ->
      "(" ++ formulaToString env f ++ " <-> " ++ formulaToString env g ++ ")"
    And xs ->
      "(" ++ concat (insertInside (map (formulaToString env) xs) " && ") ++ ")"
    Or xs ->
      "(" ++ concat (insertInside (map (formulaToString env) xs) " || ") ++ ")"
    Next f -> "(X " ++ formulaToString env f ++ ")"
    Previous f -> "(Y " ++ formulaToString env f ++ ")"
    Globally f -> "(G " ++ formulaToString env f ++ ")"
    Finally f -> "(F " ++ formulaToString env f ++ ")"
    Historically f -> "(H " ++ formulaToString env f ++ ")"
    Once f -> "(H " ++ formulaToString env f ++ ")"
    Until f g ->
      "(" ++ formulaToString env f ++ " U " ++ formulaToString env g ++ ")"
    Release f g ->
      "(" ++ formulaToString env f ++ " R " ++ formulaToString env g ++ ")"
    Weak f g ->
      "(" ++ formulaToString env f ++ " W " ++ formulaToString env g ++ ")"
    Since f g ->
      "(" ++ formulaToString env f ++ " S " ++ formulaToString env g ++ ")"
    Triggered f g ->
      "(" ++ formulaToString env f ++ " T " ++ formulaToString env g ++ ")"

-----------------------------------------------------------------------------
-- | Converts a specification to a string
specToString :: Specification -> String
specToString Specification {..} =
  "initially guarantee {" ++ formulaToString (stName symboltable) formula ++ "}"

-----------------------------------------------------------------------------
-- | Convert a TSL specification to a string
tslSpecToString :: TSLSpecification -> String
tslSpecToString TSLSpecification {..} =
  "initially assume {\n" ++
  help assumptions ++ "}\n\ninitially guarantee {\n" ++ help guarantees ++ "}"
  where
    help :: [Formula Int] -> String
    help xs =
      concatMap
        (\f -> "  " ++ formulaToString (stName tslSymboltable) f ++ ";\n")
        xs

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  TSL.ModuloTheories.ConsistencyChecking
-- Description :
-- Maintainer  :  Wonhyuk Choi
module TSL.ModuloTheories.ConsistencyChecking
  ( generateConsistencyAssumptions,
    consistencyDebug,
    ConsistencyDebugInfo (..),
  )
where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except
import TSL.Ast
  ( AstInfo (..),
    SymbolInfo (..),
    deduplicate,
  )
import TSL.Error (Error, errConsistency)
import TSL.ModuloTheories.Debug (IntermediateResults (..))
import TSL.ModuloTheories.Predicates
  ( TheoryPredicate (..),
    enumeratePreds,
    pred2Smt,
    pred2Tsl,
    predInfo,
    predTheory,
  )
import TSL.ModuloTheories.Solver (solveSat)
import TSL.ModuloTheories.Theories
  ( Theory,
    TheorySymbol,
    isUninterpreted,
    smtSortDecl,
    symbol2Smt,
    symbolType,
  )

-------------------------------------------------------------------------------

pred2Assumption :: TheoryPredicate -> String
pred2Assumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

data ConsistencyDebugInfo = ConsistencyDebugInfo IntermediateResults String
  deriving (Show)

generateConsistencyAssumptions ::
  FilePath ->
  [TheoryPredicate] ->
  [ExceptT Error IO String]
generateConsistencyAssumptions path preds =
  map ((fmap fst) . (consistencyChecking path)) (enumeratePreds preds)

consistencyDebug ::
  FilePath ->
  [TheoryPredicate] ->
  [ExceptT Error IO ConsistencyDebugInfo]
consistencyDebug path preds =
  map ((fmap snd) . (consistencyChecking path)) (enumeratePreds preds)

consistencyChecking ::
  FilePath ->
  TheoryPredicate ->
  ExceptT Error IO (String, ConsistencyDebugInfo)
consistencyChecking solverPath pred = do
  isSat <- solveSat solverPath query
  if isSat
    then except $ errConsistency $ "Predicate " ++ show pred ++ " is satisfiable."
    else
      let assumption = pred2Assumption pred
          intermediateResults =
            IntermediateResults (show pred) query (show isSat)
          debugInfo =
            ConsistencyDebugInfo intermediateResults assumption
       in return (assumption, debugInfo)
  where
    query = pred2SmtQuery pred

pred2SmtQuery :: TheoryPredicate -> String
pred2SmtQuery p = unlines [smtDeclarations, assertion, checkSat]
  where
    smtDeclarations = smtDecls (predTheory p) $ deduplicate $ predInfo p
    assertion = "(assert " ++ pred2Smt p ++ ")"
    checkSat = "(check-sat)"

smtDecls :: Theory -> AstInfo TheorySymbol -> String
smtDecls theory (AstInfo vars funcs preds) =
  unlines [logic, sortDecl, varDecls, funcDecls, predDecls]
  where
    logic = "(set-logic " ++ show theory ++ ")"
    sortDecl = smtSortDecl theory
    varDecls = unlines $ map declConst vars
    funcDecls = unlines $ map declFunc funcs
    predDecls = unlines $ map declPred preds
    declConst (SymbolInfo x _) =
      "(declare-const " ++ symbol2Smt x ++ " " ++ symbolType x ++ ")"
    declareFun retType (SymbolInfo f arity) =
      if not (isUninterpreted f)
        then ""
        else
          unwords
            [ "(declare-fun",
              symbol2Smt f,
              "(",
              unwords $ replicate arity $ show theory,
              ")",
              retType ++ ")"
            ]
    declFunc = declareFun (show theory)
    declPred = declareFun "Bool"

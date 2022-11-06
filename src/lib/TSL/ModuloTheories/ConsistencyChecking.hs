-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.ConsistencyChecking
-- Description :  
-- Maintainer  :  Wonhyuk Choi
--

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.ConsistencyChecking( generateConsistencyAssumptions
                                             , consistencyDebug
                                             ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error(Error, errConsistency)

import TSL.Ast( AstInfo(..)
              , SymbolInfo(..)
              , deduplicate
              )

import TSL.ModuloTheories.Debug (IntermediateResults(..))

import TSL.ModuloTheories.Solver (solveSat)

import TSL.ModuloTheories.Theories( Theory
                                  , TheorySymbol
                                  , symbol2Smt
                                  , symbolType
                                  , smtSortDecl
                                  , isUninterpreted
                                  )

import TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , enumeratePreds
                                       , pred2Tsl
                                       , pred2Smt
                                       , predTheory
                                       , predInfo
                                       )

-------------------------------------------------------------------------------

pred2Assumption :: TheoryPredicate -> String
pred2Assumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

generateConsistencyAssumptions
  :: FilePath
  -> [TheoryPredicate]
  -> [ExceptT Error IO String]
generateConsistencyAssumptions path preds =
  map ((fmap fst) . (consistencyChecking path)) (enumeratePreds preds)

consistencyDebug
  :: FilePath
  -> [TheoryPredicate]
  -> [ExceptT Error IO IntermediateResults]
consistencyDebug path preds =
  map ((fmap snd) . (consistencyChecking path)) (enumeratePreds preds)

consistencyChecking
  :: FilePath
  -> TheoryPredicate
  -> ExceptT Error IO (String, IntermediateResults)
consistencyChecking solverPath pred = do
  isSat <- solveSat solverPath query
  if isSat
    then except $ errConsistency $ "Predicate " ++ show pred ++ " is satisfiable."
    else 
      let assumption = pred2Assumption pred
          debugInfo  = IntermediateResults (show pred) query (show isSat) assumption
      in return (assumption, debugInfo)
  where
    query  = pred2SmtQuery pred

pred2SmtQuery :: TheoryPredicate -> String
pred2SmtQuery p = unlines [smtDeclarations, assertion, checkSat]
  where
    smtDeclarations   = smtDecls (predTheory p) $ deduplicate $ predInfo p
    assertion         = "(assert " ++ pred2Smt p ++ ")"
    checkSat          = "(check-sat)"

smtDecls :: Theory -> AstInfo TheorySymbol -> String
smtDecls theory (AstInfo vars funcs preds) =
  unlines [logic, sortDecl, varDecls, funcDecls, predDecls]
  where
    logic     = "(set-logic " ++ show theory ++ ")"
    sortDecl  = smtSortDecl theory
    varDecls  = unlines $ map declConst vars
    funcDecls = unlines $ map declFunc funcs
    predDecls = unlines $ map declPred preds
    declConst (SymbolInfo x _) =
      "(declare-const " ++ symbol2Smt x ++ " " ++ symbolType x  ++ ")"
    declareFun retType (SymbolInfo f arity) = 
      if not (isUninterpreted f)
        then ""
        else unwords
      ["(declare-fun"
      , symbol2Smt f
      , "("
      , unwords $ replicate arity $ show theory
      , ")"
      , retType ++ ")"
      ]
    declFunc = declareFun (show theory)
    declPred = declareFun "Bool"

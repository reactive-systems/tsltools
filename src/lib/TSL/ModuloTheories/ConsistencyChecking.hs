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
module TSL.ModuloTheories.ConsistencyChecking(consistencyChecking, debug) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error(Error)

import TSL.Ast(AstInfo(..), SymbolInfo(..), stringifyAst)

import TSL.ModuloTheories.Theories( Theory
                                  , TheorySymbol
                                  , symbol2Smt
                                  , symbolType
                                  , smtSortDecl
                                  )

import TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , enumeratePreds
                                       , pred2Tsl
                                       , pred2Smt
                                       , predTheory
                                       , predInfo
                                       )

-------------------------------------------------------------------------------

consistencyChecking
  :: (String -> ExceptT Error IO Bool)
  -> [TheoryPredicate]
  -> ExceptT Error IO [String]
consistencyChecking satSolver preds = (map toTslAssumption) <$> onlyUnsat
  where
    checkSat          = satSolver . pred2SmtQuery
    unsatZipFilter    = (map fst . filter (not . snd)) . (zip preds)
    onlyUnsat         = unsatZipFilter <$> (traverse checkSat preds)
    toTslAssumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

debug = pred2SmtQuery

pred2SmtQuery :: TheoryPredicate -> String
pred2SmtQuery p = unlines [smtDeclarations, assertion, checkSat]
  where
    smtDeclarations   = smtDecls (predTheory p) $ predInfo p
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
    declareFun (SymbolInfo f arity) retType = unwords
      ["(declare-fun"
      , symbol2Smt f
      , "("
      , unwords $ replicate arity $ show theory
      , ")"
      , retType ++ ")"
      ]
    declFunc = flip declareFun (show theory)
    declPred = flip declareFun "Bool"

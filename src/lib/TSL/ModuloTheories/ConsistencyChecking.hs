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
module TSL.ModuloTheories.ConsistencyChecking( consistencyChecking
                                             , consistencyDebug
                                             ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error(Error)

import TSL.Ast( AstInfo(..)
              , SymbolInfo(..)
              , deduplicate
              )

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

eof :: String
eof = ";; END OF FILE\n\n"

pred2TslAssumption :: TheoryPredicate -> String
pred2TslAssumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

consistencyDebug
  :: (String -> ExceptT Error IO Bool)
  -> [TheoryPredicate]
  -> ExceptT Error IO [(String, String, Maybe String)]
consistencyDebug satSolver preds = (map assumeOnlyUnsat) <$> zippedResults
  where
    preds'                       = enumeratePreds preds
    queries                      = map pred2SmtQuery preds'
    zippedResults                = fmap (zip3 preds' queries) $ traverse satSolver queries
    assumeOnlyUnsat (p, q, res)  =
      if res then (show p, q, Nothing) else (show p, q, Just (pred2TslAssumption p))

consistencyChecking
  :: (String -> ExceptT Error IO Bool)
  -> [TheoryPredicate]
  -> ExceptT Error IO [String]
consistencyChecking satSolver preds = (map pred2TslAssumption) <$> onlyUnsat
  where
    checkSat          = satSolver . pred2SmtQuery
    preds'            = enumeratePreds preds
    unsatZipFilter    = (map fst . filter (not . snd)) . (zip preds')
    onlyUnsat         = unsatZipFilter <$> (traverse checkSat preds')

pred2SmtQuery :: TheoryPredicate -> String
pred2SmtQuery p = unlines [smtDeclarations, assertion, checkSat, eof]
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

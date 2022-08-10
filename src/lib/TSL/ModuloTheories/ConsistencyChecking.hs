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
module TSL.ModuloTheories.ConsistencyChecking(consistencyChecking) where

-------------------------------------------------------------------------------

import Control.Monad(filterM)

import TSL.Error(Error)

import TSL.Ast(stringifyAst)

import TSL.ModuloTheories.Theories(Theory, symbol2Smt, symbolType)

import TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , enumeratePreds
                                       , pred2Tsl
                                       , pred2Smt
                                       , predTheory
                                       , getPredVars
                                       )

-------------------------------------------------------------------------------

consistencyChecking
  :: (String -> IO (Either Error Bool))
  -> [TheoryPredicate]
  -> IO (Either Error [String])
consistencyChecking satSolver preds = fmap (fmap (map toTslAssumption)) onlyUnsat
  where
    preds'            = enumeratePreds preds
    checkSat          = satSolver . pred2SmtQuery
    unsatZipFilter    = (map fst . filter (not . snd)) . (zip preds')
    filterHelperM     = return . (fmap unsatZipFilter) . sequence
    onlyUnsat         = (traverse checkSat preds') >>= filterHelperM
    toTslAssumption p = "G " ++ pred2Tsl (NotPLit p) ++ ";"

pred2SmtQuery :: TheoryPredicate -> String
pred2SmtQuery p = unlines $ [logic, variables, assert, checkSAT]
  where
    logic       = "(set-logic " ++ show (predTheory p) ++ ")"
    variables   = unlines $ map declConst $ getPredVars p
    assert      = "(assert " ++ pred2Smt p ++ ")"
    checkSAT    = "(check-sat)"
    declConst x =
      "(declare-const " ++ symbol2Smt x ++ " " ++ symbolType x ++ ")"

-- -- (set-logic LIA)
-- -- (declare-const vruntime2 Int)
-- -- (declare-const vruntime1 Int)

-- -- (assert (and (not (> vruntime2 vruntime1)) (not (> vruntime2 vruntime1))))
-- -- (check-sat)

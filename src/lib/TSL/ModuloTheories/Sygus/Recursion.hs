-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Recursion
-- Description :  Handles the recursive query logic for SyGuS.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Recursion
  ( generatePbeDtos
  , findRecursion
  , config_SUBQUERY_AST_MAX_SIZE
  ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error (Error, errSygus, parseError)

import TSL.ModuloTheories.Cfg ( Cfg(..)
                              , outputSignals
                              , extendCfg
                              )

import TSL.ModuloTheories.Predicates( TheoryPredicate
                                    , pred2Smt
                                    , predTheory
                                    , predSignals
                                    , predReplacedSmt
                                    )

import TSL.ModuloTheories.Theories( TheorySymbol
                                  , TAst
                                  , Theory
                                  , tastSignals
                                  , tast2Smt
                                  , symbolType
                                  , symbolTheory
                                  , smtSortDecl
                                  , makeSignal
                                  )

import TSL.ModuloTheories.Solver (runSolver)

import TSL.ModuloTheories.Sygus.Common( Dto (..)
                                      , Temporal (..)
                                      , Model (..)
                                      , IntermediateResults
                                      , targetPostfix
                                      , parenthize
                                      )

import TSL.ModuloTheories.Sygus.Update (Update (..), DataSource (..))

import TSL.ModuloTheories.Sygus.Parser (parseModels)

-------------------------------------------------------------------------------
-- (set-logic LIA)
-- (set-option :produce-models true)
-- (declare-const vruntime2 Int)
-- (declare-const vruntime1 Int)
-- 
-- (assert (not (= vruntime1 1)))
-- (assert (not (> vruntime1 vruntime2)))
-- (check-sat)
-- (get-model)
--
-- 1. Produce model queries
-- 2. Run queries
-- 3. Get results
-- 4. Use these to create SyGuS queries
-- 5. Find recursion inside of them

config_NUM_RECURSIVE_SUBQUERIES :: Int
config_NUM_RECURSIVE_SUBQUERIES = 3

config_SUBQUERY_AST_MAX_SIZE :: Int
config_SUBQUERY_AST_MAX_SIZE = 3

model2SmtPred :: (Show a) => Model a -> String
model2SmtPred (Model (symbol, model)) =
  parenthize 1 $ unwords [ "="
                         , show symbol
                         , show model
                         ]

produceModelsQuery :: (Show a) => [Model a] -> Theory -> TheoryPredicate -> String
produceModelsQuery models theory pred = unlines [ header
                                                , setOption
                                                , declareConstList
                                                , notTheseModels
                                                , assertion $ pred2Smt pred
                                                , footer
                                                ]
  where paren1           = parenthize 1
        header           = paren1 $ unwords ["set-logic", show theory]
        setOption        = "(set-option :produce-models true)"
        footer           = "(check-sat)\n(get-model)"
        declareConst var = paren1 $ unwords ["declare-const ", show var, symbolType var]
        declareConstList = unlines $ map declareConst $ predSignals pred
        assertion stmt   = paren1 $ unwords ["assert", stmt]
        notTheseModels   = unlines $ map (assertion . model2SmtPred) models

runGetModel :: FilePath -> String -> IO String
runGetModel solverPath = runSolver solverPath args
  where args = ["--lang=smt2"]

modifyPredicate :: [Model String] -> TheoryPredicate -> TheoryPredicate
modifyPredicate = undefined

generatePbeDtos :: FilePath -> Dto -> Bool -> ExceptT Error IO [(Dto, Maybe IntermediateResults)]
generatePbeDtos = undefined

findRecursion :: [[[Update a]]] -> Either Error [[Update a]]
findRecursion = undefined 

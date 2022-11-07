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

import Control.Monad (liftM2)

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
                                    , replacePredicate
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
                                  , read2Symbol
                                  )

import TSL.ModuloTheories.Solver (runGetModel)

import TSL.ModuloTheories.Debug (IntermediateResults(..))

import TSL.ModuloTheories.Sygus.Common( Dto (..)
                                      , Temporal (..)
                                      , Model (..)
                                      , targetPostfix
                                      , parenthize
                                      )

import TSL.ModuloTheories.Sygus.Update (Update (..), DataSource (..))

import TSL.ModuloTheories.Sygus.Parser (parseModels)

import Debug.Trace (trace)

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

config_NUM_SUBQUERIES :: Int
config_NUM_SUBQUERIES = 3

config_SUBQUERY_AST_MAX_SIZE :: Int
config_SUBQUERY_AST_MAX_SIZE = 3

model2SmtPred :: (Show a) => Model a -> String
model2SmtPred (Model (symbol, model)) =
  parenthize 1 $ unwords [ "="
                         , show symbol
                         , show model
                         ]

produceModelsQuery :: (Show a) => [Model a] -> TheoryPredicate -> String
produceModelsQuery models pred = unlines [ header
                                         , setOption
                                         , declareConstList
                                         , notTheseModels
                                         , assertion $ pred2Smt pred
                                         , footer
                                         ]
  where paren1           = parenthize 1
        header           = paren1 $ unwords ["set-logic", show (predTheory pred)]
        setOption        = "(set-option :produce-models true)"
        footer           = "(check-sat)\n(get-model)"
        declareConst var = paren1 $ unwords ["declare-const ", show var, symbolType var]
        declareConstList = unlines $ map declareConst $ predSignals pred
        assertion stmt   = paren1 $ unwords ["assert", stmt]
        notTheseModels   = unlines $ map (assertion . model2SmtPred) models

modifyPredicate
  :: [Model String]
  -> TheoryPredicate
  -> Either Error TheoryPredicate
modifyPredicate models pred = theoryModels >>= (return . foldr modify pred)
  where
    sequence' :: [Model (Either e a)] -> Either e [Model a]
    sequence' = \case
      []                 -> Right []
      ((Model (k,v)):xs) -> case k of 
                      Left err -> Left err
                      Right k' -> case v of
                                    Left err -> Left err
                                    Right v' ->
                                      fmap ((Model (k', v')):) $ sequence' xs

    theoryModels :: Either Error [Model TheorySymbol]
    theoryModels =
        sequence' $ map (fmap (read2Symbol (predTheory pred))) models

    modify :: Model TheorySymbol -> TheoryPredicate -> TheoryPredicate
    modify (Model replacer) = replacePredicate replacer

generatePbeModel
    :: FilePath
    -> TheoryPredicate
    -> [Model String]
    -> ExceptT Error IO (Model String, IntermediateResults)
generatePbeModel solverPath pred prevModels = liftM2 (,) model debugInfo
  where 
    query :: String
    query = produceModelsQuery prevModels pred

    result :: ExceptT Error IO String
    result = runGetModel solverPath query

    model :: ExceptT Error IO (Model String)
    model = do
      string <- result
      case parseModels string of
        Left err    -> except $ Left err
        Right model -> return $ model

    debugInfo :: ExceptT Error IO IntermediateResults
    debugInfo = do
      runResult   <- result
      parsedModel <- model
      return $ IntermediateResults query runResult (show parsedModel)

generatePbeDtos
  :: FilePath
  -> Dto
  -> ExceptT Error IO [(Dto, IntermediateResults)]
generatePbeDtos solverPath (Dto theory pre post) = do
  (_, dtos, debugInfos) <- looper config_NUM_SUBQUERIES (return nullInit)
  return $ zip dtos debugInfos

  where
    nullInit :: ([Model String], [Dto], [IntermediateResults])
    nullInit = ([], [], [])

    genNewDto :: [Model String] -> Either Error Dto
    genNewDto models = do
      newPreCondition <- modifyPredicate models pre
      return $ Dto theory newPreCondition post
    
    looper
      :: Int
      -> ExceptT Error IO ([Model String], [Dto], [IntermediateResults])
      -> ExceptT Error IO ([Model String], [Dto], [IntermediateResults])
    looper 0        prev = prev
    looper numIters prev = do
      (models, dtos, debugInfos) <- prev
      (newModel, newInfo) <- generatePbeModel solverPath pre models
      let updatedModels = newModel:models
      newDto <- except $ genNewDto updatedModels
      let updatedDto    = newDto:dtos
          updatedInfos  = newInfo:debugInfos
      looper (numIters - 1) $ return (updatedModels, updatedDto, updatedInfos)

findRecursion :: [[[Update a]]] -> Either Error [[Update a]]
findRecursion = undefined 

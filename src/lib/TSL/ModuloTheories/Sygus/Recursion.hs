-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Recursion
-- Description :  Handles the recursive query logic for SyGuS.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Recursion
  ( generatePbeModels
  , findRecursion
  , config_SUBQUERY_AST_MAX_SIZE
  ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Control.Monad (liftM2)

import TSL.Error (Error, errSygus)

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

config_NUM_SUBQUERIES :: Int
config_NUM_SUBQUERIES = 3

config_SUBQUERY_AST_MAX_SIZE :: Int
config_SUBQUERY_AST_MAX_SIZE = 3

produceModelsQuery :: (Show a) => [[Model a]] -> TheoryPredicate -> String
produceModelsQuery models pred = query
  where paren1            = parenthize 1
        header            = paren1 $ unwords ["set-logic", show (predTheory pred)]
        setOption         = "(set-option :produce-models true)"
        footer            = "(check-sat)\n(get-model)"
        declareConst var  = paren1 $ unwords ["declare-const ", show var, symbolType var]
        declareConstList  = unlines $ map declareConst $ predSignals pred
        assertion stmt    = paren1 $ unwords ["assert", stmt]
        notTheseModels ms = assertion
                              $ paren1 $ ("not "++)
                              $ paren1 $ unwords $ ("and":)
                              $ map show ms
        query             = unlines [ header
                                    , setOption
                                    , declareConstList
                                    , unlines $ map notTheseModels models
                                    , assertion $ pred2Smt pred
                                    , footer
                                    ]

generatePbeModel
    :: FilePath
    -> TheoryPredicate
    -> [[Model String]]
    -> ExceptT Error IO ([Model String], IntermediateResults)
generatePbeModel solverPath pred prevModels = liftM2 (,) models debugInfo
  where 
    query :: String
    query = produceModelsQuery prevModels pred

    result :: ExceptT Error IO String
    result = runGetModel solverPath query

    models :: ExceptT Error IO [Model String]
    models = do
      string <- result
      case parseModels string of
        Left err    -> except $ Left err
        Right model -> return $ model

    debugInfo :: ExceptT Error IO IntermediateResults
    debugInfo = do
      runResult    <- result
      parsedModels <- models
      return $ IntermediateResults query runResult (show parsedModels)

generatePbeModels
  :: FilePath
  -> Dto
  -> ExceptT Error IO [([Model TheorySymbol], IntermediateResults)]
generatePbeModels solverPath (Dto theory pre _) = do
  (models, debugInfos) <- looper config_NUM_SUBQUERIES (return nullInit)
  theoryModels         <- except $ sequence $ map sequence $ map (map readModel) models
  return $ zip theoryModels debugInfos

  where
    nullInit :: ([[Model String]], [IntermediateResults])
    nullInit = ([], [])

    readModel :: Model String -> Either Error (Model TheorySymbol)
    readModel (Model (k, v)) =
      case read2Symbol theory k of
        Left err -> Left err
        Right k' -> case read2Symbol theory v of
                      Left err -> Left err
                      Right v' -> Right $ Model (k', v')
    
    looper
      :: Int
      -> ExceptT Error IO ([[Model String]], [IntermediateResults])
      -> ExceptT Error IO ([[Model String]], [IntermediateResults])
    looper 0        prev = prev
    looper numIters prev = do
      (models, debugInfos) <- prev
      (newModel, newInfo)  <- generatePbeModel solverPath pre models
      let updatedInfos  = newInfo:debugInfos
          updatedModels = newModel:models
      looper (numIters - 1) $ return (updatedModels, updatedInfos)

-- (a -> m b) -> t a -> m (t b)
findRecursion :: (Eq a, Show a) => [[[Update a]]] -> Either Error [[Update a]]
findRecursion subqueryUpdates = do
  flattened <- mapM flattenUpdates subqueryUpdates
  extracted <- mapM extractRecursion flattened
  recursion <- extractRecursion extracted
  return $ [[recursion]]
  where
    debugUpdates :: String
    debugUpdates = show subqueryUpdates

    extractRecursion :: (Eq a) => [Update a] -> Either Error (Update a)
    extractRecursion []     = errSygus $
                                "Empty updates found for " ++ debugUpdates
    extractRecursion (x:xs) = if all (==x) xs
                                 then return x
                                 else errSygus $ 
                                        "Updates not recursive: " ++
                                        debugUpdates

    flattenUpdates :: [[a]] -> Either Error [a]
    flattenUpdates []     = Right []
    flattenUpdates (x:xs) = case x of
      []  -> errSygus $ "Empty updates found for " ++ debugUpdates
      [y] -> fmap (y:) (flattenUpdates xs)
      _   -> errSygus $ "Too many updates in one time: " ++ debugUpdates

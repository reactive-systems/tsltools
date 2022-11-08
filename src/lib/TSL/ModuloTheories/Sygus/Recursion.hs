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
  ( generatePbeDtos
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

config_NUM_SUBQUERIES :: Int
config_NUM_SUBQUERIES = 3

config_SUBQUERY_AST_MAX_SIZE :: Int
config_SUBQUERY_AST_MAX_SIZE = 3

model2SmtPred :: (Show a) => Model a -> String
model2SmtPred (Model (symbol, model)) =
  parenthize 1 $ filter (/='\"') $ unwords [ "="
                                           , show symbol
                                           , show model
                                           ]

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
                              $ map model2SmtPred ms
        query             = unlines [ header
                                    , setOption
                                    , declareConstList
                                    , unlines $ map notTheseModels models
                                    , assertion $ pred2Smt pred
                                    , footer
                                    ]

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

generatePbeDtos
  :: FilePath
  -> Dto
  -> ExceptT Error IO [(Dto, IntermediateResults)]
generatePbeDtos solverPath (Dto theory pre post) = do
  (_, dtos, debugInfos) <- looper config_NUM_SUBQUERIES (return nullInit)
  return $ zip dtos debugInfos

  where
    nullInit :: ([[Model String]], [Dto], [IntermediateResults])
    nullInit = ([], [], [])

    genNewDto :: [Model String] -> Either Error Dto
    genNewDto models = Dto theory <$> (modify pre) <*> (modify post)
      where modify = modifyPredicate models
    
    looper
      :: Int
      -> ExceptT Error IO ([[Model String]], [Dto], [IntermediateResults])
      -> ExceptT Error IO ([[Model String]], [Dto], [IntermediateResults])
    looper 0        prev = prev
    looper numIters prev = do
      (models, dtos, debugInfos) <- prev
      (newModel, newInfo) <- generatePbeModel solverPath pre models
      newDto <- except $ genNewDto newModel
      let updatedDto    = newDto:dtos
          updatedInfos  = newInfo:debugInfos
          updatedModels = newModel:models
      looper (numIters - 1) $ return (updatedModels, updatedDto, updatedInfos)

findRecursion :: (Eq a, Show a) => [[[Update a]]] -> Either Error [[Update a]]
findRecursion subqueryUpdates = do
  flattened <- mapM flattenUpdates subqueryUpdates
  extracted <- mapM extractRecursion flattened
  return [extracted]
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

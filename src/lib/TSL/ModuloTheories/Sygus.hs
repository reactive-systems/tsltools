-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus ( generateSygusAssumptions
                                , SygusDebugInfo (..)
                                , buildDtoList
                                ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Data.Text(pack, unpack, replace)

import Data.List (isInfixOf)

import Control.Monad (liftM2)

import Control.Exception(assert)

import TSL.Error (Error, errSygus, parseError)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Predicates (predTheory, TheoryPredicate)

import TSL.ModuloTheories.Solver (runSygusQuery)

import TSL.ModuloTheories.Debug (IntermediateResults(..))

import TSL.ModuloTheories.Sygus.Common ( Temporal(..)
                                       , Term
                                       , Dto (..)
                                       , targetPostfix
                                       )

import TSL.ModuloTheories.Sygus.Query (generateSygusQuery)

import TSL.ModuloTheories.Sygus.Parser (parseSygusResult)

import TSL.ModuloTheories.Sygus.Assumption (makeAssumption)

import TSL.ModuloTheories.Sygus.Update (Update, term2Updates)

import TSL.ModuloTheories.Sygus.Recursion ( generatePbeDtos
                                          , findRecursion
                                          , config_SUBQUERY_AST_MAX_SIZE
                                          )

-------------------------------------------------------------------------------

data SygusDebugInfo =
    NextDebug IntermediateResults
  | EventuallyDebug [(IntermediateResults, IntermediateResults)]
  deriving (Show)

temporalAtoms :: [Temporal]
temporalAtoms = [Next 1]

removePostfix :: String -> String
removePostfix = unpack . replace postfix "" . pack
  where postfix = pack targetPostfix

buildDto :: TheoryPredicate -> TheoryPredicate -> Dto
buildDto pre post = Dto theory pre post
  where theory   = assert theoryEq $ predTheory pre
        theoryEq = (predTheory pre) == (predTheory post)

buildDtoList :: [TheoryPredicate] -> [Dto]
buildDtoList preds = concat $ map buildWith preds
  where buildWith pred = map (buildDto pred) preds

generateUpdates
  :: FilePath
  -> Cfg
  -> Int
  -> Dto
  -> Bool
  -> ExceptT Error IO ([[Update String]], Maybe IntermediateResults)
generateUpdates solverPath cfg depth dto debug =
  liftM2 (,) updates debugInfo
  where
    query :: Either Error String
    query = generateSygusQuery cfg dto

    result :: ExceptT Error IO String
    result = except query >>= (runSygusQuery solverPath depth)

    term :: ExceptT Error IO (Term String)
    term = do
        value <- result
        case parseSygusResult value of
          Left err   -> except $ parseError err
          Right term -> return term

    updates :: ExceptT Error IO [[Update String]]
    updates = term2Updates <$> term

    debugInfo :: ExceptT Error IO (Maybe IntermediateResults)
    debugInfo =
      if debug
        then Just <$> (IntermediateResults (show dto) <$> except query <*> result)
        else return Nothing

generateAssumption
  :: FilePath
  -> Cfg
  -> Dto
  -> Temporal
  -> Bool
  -> ExceptT Error IO (String, Maybe SygusDebugInfo)
generateAssumption solverPath cfg dto temporal debug = case temporal of
  Next maxAstSize -> do
    (updates, debugInfo) <- genUpdates maxAstSize dto
    let assumption = makeAssumption' updates
        debugInfo' = return $ if debug
                        then Just $ NextDebug $ unwrapIntermediateResult debugInfo
                        else Nothing
    liftM2 (,) assumption debugInfo'

  Eventually -> do
    pbeResults <- generatePbeDtos solverPath dto debug
    let (pbeDtos, pbeInfos) = unzip pbeResults

    subqueryResults <- mapM (genUpdates config_SUBQUERY_AST_MAX_SIZE) pbeDtos
    let (subqueryUpdates, subqueryInfos) = unzip subqueryResults

    updates    <- except $ findRecursion subqueryUpdates
    assumption <- makeAssumption' updates
    let debugInfo = if debug
                       then Nothing
                       else Just $ EventuallyDebug $ zipWith unwraps pbeInfos subqueryInfos
    return (assumption, debugInfo)

  where
    genUpdates depth dto'   = generateUpdates solverPath cfg depth dto' debug 
    makeAssumption' updates = except $ makeAssumption dto temporal updates

    unwrapIntermediateResult = \case
      Nothing -> error "Unwrap called for Nothing!"
      Just intermediateResult -> intermediateResult

    unwraps a b = (unwrapIntermediateResult a, unwrapIntermediateResult b)

generateSygusAssumptions
  :: FilePath
  -> Cfg
  -> [Dto]
  -> Bool
  -> [ExceptT Error IO (String, Maybe SygusDebugInfo)]
generateSygusAssumptions solverPath cfg dtos debug = genAssumption <$> dtos <*> temporalAtoms
  where genAssumption dto temporal = generateAssumption solverPath cfg dto temporal debug

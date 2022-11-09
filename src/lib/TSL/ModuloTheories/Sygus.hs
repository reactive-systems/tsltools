-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus ( generateSygusAssumptions
                                , SygusDebugInfo (..)
                                , buildDtoList
                                , sygusDebug
                                ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Control.Monad (liftM2)

import Control.Exception(assert)

import TSL.Error (Error, errSygus)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Predicates (predTheory, TheoryPredicate)

import TSL.ModuloTheories.Solver (runSygusQuery)

import TSL.ModuloTheories.Debug (IntermediateResults(..))

import TSL.ModuloTheories.Theories (Theory, TheorySymbol, sygus2Supported)

import TSL.ModuloTheories.Sygus.Common ( Temporal(..)
                                       , Term
                                       , Dto (..)
                                       , Model
                                       )

import TSL.ModuloTheories.Sygus.Query (generateSygusQuery)

import TSL.ModuloTheories.Sygus.Parser (parseSygusResult)

import TSL.ModuloTheories.Sygus.Assumption (makeAssumption)

import TSL.ModuloTheories.Sygus.Update (Update, term2Updates)

import TSL.ModuloTheories.Sygus.Recursion ( generatePbeModels
                                          , findRecursion
                                          , config_SUBQUERY_AST_MAX_SIZE
                                          )
import Debug.Trace (trace)

-------------------------------------------------------------------------------

data SygusDebugInfo =
    NextDebug IntermediateResults String
  | EventuallyDebug [(IntermediateResults, IntermediateResults)] String
  deriving (Show)

temporalAtoms :: [Temporal]
temporalAtoms = [Next 1, Eventually]

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
  -> [Model TheorySymbol]
  -> Dto
  -> ExceptT Error IO ([[Update String]], IntermediateResults)
generateUpdates solverPath cfg depth models dto = liftM2 (,) updates debugInfo
  where
    query :: Either Error String
    query = generateSygusQuery cfg models dto

    result :: ExceptT Error IO String
    result = except query >>= (runSygusQuery solverPath depth)

    term :: ExceptT Error IO (Term String)
    term = do
        value <- result
        case parseSygusResult (head (lines value)) of
          Left err   -> except $ Left err
          Right term -> return term

    updates :: ExceptT Error IO [[Update String]]
    updates = term2Updates <$> term

    debugInfo :: ExceptT Error IO IntermediateResults
    debugInfo = IntermediateResults (show dto) <$> except query <*> result

generateAssumption
  :: FilePath
  -> Cfg
  -> Dto
  -> Temporal
  -> ExceptT Error IO (String, SygusDebugInfo)
generateAssumption solverPath cfg dto temporal =
  if not $ sygus2Supported $ theory dto
  then except unsupportedError
  else case temporal of
    next@(Next maxAstSize) -> do
      (updates, debugInfo) <- genNextUpdates maxAstSize
      let assumption = makeAssumption' next updates
          debugInfo' = NextDebug debugInfo <$> assumption
      liftM2 (,) assumption debugInfo'

    Eventually -> do
      pbeResults <- generatePbeModels solverPath dto
      let (pbeModels, pbeInfos) = unzip pbeResults

      subqueryResults <- mapM genEventuallyUpdates pbeModels
      let (subqueryUpdates, subqueryInfos) = unzip subqueryResults

      updates    <- except $ findRecursion subqueryUpdates
      assumption <- makeAssumption' Eventually updates
      let debugInfo = EventuallyDebug (zip pbeInfos subqueryInfos) assumption
      return (assumption, debugInfo)

  where
    genUpdates depth     = (flip (generateUpdates solverPath cfg depth)) dto
    genNextUpdates       = (flip genUpdates) []
    genEventuallyUpdates = genUpdates config_SUBQUERY_AST_MAX_SIZE
    unsupportedError     = errSygus $
                             "Theory " ++
                             show (theory dto) ++ 
                             " not supported in the Sygus 2 Standard"
    makeAssumption' temporal updates =
      except $ makeAssumption dto temporal updates

generateSygusAssumptions
  :: FilePath
  -> Cfg
  -> [Dto]
  -> [ExceptT Error IO String]
generateSygusAssumptions solverPath cfg dtos =
    map (fmap fst) $
      (generateAssumption solverPath cfg) <$>
      dtos <*>
      temporalAtoms

sygusDebug
  :: FilePath
  -> Cfg
  -> [Dto]
  -> [ExceptT Error IO SygusDebugInfo]
sygusDebug solverPath cfg dtos =
    map (fmap snd) $
      (generateAssumption solverPath cfg) <$>
      dtos <*>
      temporalAtoms

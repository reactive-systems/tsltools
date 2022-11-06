-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus ( generateSygusAssumptions
                                , SygusDebugInfo (..)
                                , buildDtoList
                                , sygusDebug
                                ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Data.Text(pack, unpack, replace)

import Data.List (isInfixOf)

import Control.Monad (liftM, liftM2)

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
    NextDebug IntermediateResults String
  | EventuallyDebug [(IntermediateResults, IntermediateResults)] String
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
  -> ExceptT Error IO ([[Update String]], IntermediateResults)
generateUpdates solverPath cfg depth dto =
  liftM2 (,) updates debugInfo
  where
    query :: Either Error String
    query = generateSygusQuery cfg dto

    result :: ExceptT Error IO String
    result = except query >>= (runSygusQuery solverPath depth)

    term :: ExceptT Error IO (Term String)
    term = do
        value <- result
        case parseSygusResult (head (lines value)) of
          Left err   -> except $ parseError err
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
generateAssumption solverPath cfg dto = \case
  next@(Next maxAstSize) -> do
    (updates, debugInfo) <- genUpdates maxAstSize dto
    let assumption = makeAssumption' next updates
        debugInfo' = NextDebug debugInfo <$> assumption
    liftM2 (,) assumption debugInfo'

  Eventually -> do
    pbeResults <- generatePbeDtos solverPath dto
    let (pbeDtos, pbeInfos) = unzip pbeResults

    subqueryResults <- mapM (genUpdates config_SUBQUERY_AST_MAX_SIZE) pbeDtos
    let (subqueryUpdates, subqueryInfos) = unzip subqueryResults

    updates    <- except $ findRecursion subqueryUpdates
    assumption <- makeAssumption' Eventually updates
    let debugInfo = EventuallyDebug (zip pbeInfos subqueryInfos) assumption
    return (assumption, debugInfo)

  where
    genUpdates depth dto'   = generateUpdates solverPath cfg depth dto'
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

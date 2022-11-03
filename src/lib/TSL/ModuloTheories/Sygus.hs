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
module TSL.ModuloTheories.Sygus ( generateAssumption
                                , SygusDebugInfo (..)
                                ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Control.Monad (liftM2)

import Data.Text(pack, unpack, replace)

import Data.List (isInfixOf)

import Control.Exception(assert)

import TSL.Error (Error, errSygus, parseError)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Predicates (predTheory, TheoryPredicate)

import TSL.ModuloTheories.Solver (runSolver)

import TSL.ModuloTheories.Sygus.Common ( Temporal(..)
                                       , Term
                                       , Dto (..)
                                       , IntermediateResults
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
  -> ExceptT Error IO [[Update String]]
generateUpdates solverPath cfg depth dto = term2Updates <$> term
  where
    query :: Either Error String
    query = generateSygusQuery cfg dto

    result :: ExceptT Error IO String
    result = except query >>= (runSygusQuery solverPath depth)

    term :: ExceptT Error IO (Term String)
    term = do
        value <- result
        return value
        case parseSygusResult value of
          Left err   -> except $ parseError err
          Right term -> return term

-- makeAssumption :: Temporal -> Dto -> [[Update String]] -> Either Error String

generateAssumption :: FilePath -> Cfg -> Dto -> Temporal -> ExceptT Error IO String
generateAssumption solverPath cfg dto = \case
  next@(Next numNext) -> except . makeAssumption next dto =<< genUpdates numNext dto
  Eventually          -> except . (makeAssumption Eventually dto) =<< eventuallyUpdates
  where
    genUpdates        = generateUpdates solverPath cfg
    pbeDtos           = generatePbeDtos solverPath dto
    pbeUpdates        = pbeDtos >>= (traverse (genUpdates config_SUBQUERY_AST_MAX_SIZE))
    eventuallyUpdates = pbeUpdates >>= (except . findRecursion)

runSygusQuery :: FilePath -> Int -> String -> ExceptT Error IO String
runSygusQuery solverPath depth = ExceptT . (fmap getResult) . (runSolver solverPath args)
  where 
    args             = depthLimit:["-o", "sygus-sol-gterm", "--lang=sygus2"]
    depthLimit       = "--sygus-abort-size=" ++ show depth
    getResult result = if isInfixOf "error" result
                          then errSygus result
                          else Right result

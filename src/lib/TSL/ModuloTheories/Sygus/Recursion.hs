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
  ( recursiveQuery
  ) where

-------------------------------------------------------------------------------

import qualified Data.Set as Set

import Data.Set (Set)

import qualified Data.Map as Map

import Control.Exception(assert)

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
                                  , tastSignals
                                  , tast2Smt
                                  , symbolType
                                  , symbolTheory
                                  , smtSortDecl
                                  , makeSignal
                                  )

import TSL.ModuloTheories.Sygus.Common( Dto(..), Temporal(..), targetPostfix)

-------------------------------------------------------------------------------

recursiveQuery :: Cfg -> Dto -> Either Error String
recursiveQuery = undefined

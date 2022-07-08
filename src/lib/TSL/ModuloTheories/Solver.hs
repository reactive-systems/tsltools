-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Solver
-- Description :  Utilities to send SMT and SyGuS problems to a solver
--                and parse their results.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Solver() where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories(Theory)

import TSL.ModuloTheories.PredicateList(PredicateLiteral)

-------------------------------------------------------------------------------

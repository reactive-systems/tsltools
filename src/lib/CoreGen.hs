-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates and unsat / unrealizabilty core
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module CoreGen
  ( generateCore
  , Core(..)
  ) where

-----------------------------------------------------------------------------
import Data.Set as Set
  ( Set
  , difference
  , fromList
  , insert
  , member
  , notMember
  , size
  , toList
  , unions
  )

import TSL
  ( TSLSpecification(..)
  , conjunctFormulas
  , constantTrue
  , disjunctFormulas
  , getOutputs
  , getPossibleUpdates
  , getUpdates
  )

import External.Context (Context, tslSpecRealizable, tslSpecSatisfiable)

-----------------------------------------------------------------------------
--
-- This represents some query (potential core)
--
type Query = TSLSpecification

-----------------------------------------------------------------------------
--
-- Given a TSL Specification generates a list of queries to find the core
--
getCores :: TSLSpecification -> [Query]
getCores tsl@TSLSpecification {guarantees = g} =
  fmap
    (\indices -> tsl {guarantees = choose indices})
    (sortedPowerSet $ length g)
  where
    choose indices = choosen ++ [otherUpdates]
      where
        choosen =
          fmap snd $ Prelude.filter (\(a, _) -> member a indices) $ zip [0 ..] g
        otherUpdates =
          disjunctFormulas $
          constantTrue :
          (Set.toList $
           Set.difference
             (getPossibleUpdates
                (conjunctFormulas g)
                (unions $ fmap getOutputs choosen))
             (getUpdates (conjunctFormulas g)))

-----------------------------------------------------------------------------
--
-- Computes the powerset (in list form) sorted by by length of the set
-- Note that this is not done by powerset and then sort to do it on the fly
--
sortedPowerSet :: Int -> [Set Int]
sortedPowerSet n = powerSetB n n
  where
    powerSetB :: Int -> Int -> [Set Int]
    powerSetB n bound
      | n < 1 = []
      | n == 1 = [fromList [i] | i <- [0 .. bound - 1]]
      | otherwise =
        let sub = powerSetB (n - 1) bound
            subNew =
              concatMap
                (\s -> [insert i s | i <- [0 .. bound - 1], notMember i s])
                (Prelude.filter (\s -> size s == n - 1) sub)
            new = toList (fromList subNew)
         in sub ++ new

-----------------------------------------------------------------------------
--
--Notion of different core types
-- 
data Core
  = NaC
  | Unsat TSLSpecification
  | Unrez TSLSpecification

-----------------------------------------------------------------------------
--
-- Generates (eventually) a core given a TSL Specification using a simple
-- realizabilty test
--
generateCore :: Context -> TSLSpecification -> IO Core
generateCore context tsl = do
  let queries = getCores tsl
  genCore' queries
  where
    genCore' :: [Query] -> IO Core
    genCore' [] = return NaC
    genCore' (q:qr) = do
      sat <- tslSpecSatisfiable context q
      if not sat
        then return (Unsat q)
        else do
          rel <- tslSpecRealizable context q
          if not rel
            then return (Unrez q)
            else do
              genCore' qr

-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen.CoreGen
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generates and unsat / unrealizabilty core
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module CoreGen.CoreGen
  ( Query
  , getCores
  ) where

-----------------------------------------------------------------------------
import Data.Set
import TSL.FormulaUtils (getOutputs, getPossibleUpdates, getUpdates)
import TSL.Logic (Formula(..))
import TSL.Specification (TSLSpecification(..))

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
          Or $
          TTrue :
          (Data.Set.toList $
           Data.Set.difference
             (getPossibleUpdates (And g) (unions $ fmap getOutputs choosen))
             (getUpdates (And g)))

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

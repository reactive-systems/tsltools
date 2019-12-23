-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen.CoreGen
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Generatse and unsat / unrealizabilty core
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-- TODO
-- - fix to right core gen
-----------------------------------------------------------------------------
module CoreGen.CoreGen
  ( Query(..)
  , getCores
  ) where

import Data.Set

import TSL.Specification (TSLSpecification(..), tslSpecToSpec)

import TSL.ToString (tslSpecToString)

import TSL.TLSF (toTLSF)

data Query =
  Query
    { potCore :: TSLSpecification
    , synthSpec :: String
    }

genQuery :: TSLSpecification -> Query
genQuery spec =
  Query {potCore = spec, synthSpec = toTLSF "CoreCandidat" (tslSpecToSpec spec)}

getCores :: TSLSpecification -> [Query]
getCores tsl@TSLSpecification {guarantees = g} =
  fmap
    (\indices -> genQuery $ tsl {guarantees = choose indices})
    (sortedPowerSet $ length g)
  where
    choose indices =
      fmap snd $ Prelude.filter (\(a, _) -> member a indices) $ zip [0 ..] g

sortedPowerSet :: Int -> [Set Int]
sortedPowerSet n = powerSetB n n

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

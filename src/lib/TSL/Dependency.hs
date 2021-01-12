-----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

-----------------------------------------------------------------------------

module TSL.Dependency
  ( DependencyRepresentation(..)
  , specifications2dependencies
  ) where

-----------------------------------------------------------------------------

import TSL.Logic (Formula(..), tslFormula)

import TSL.Specification (Specification(..))

import TSL.SymbolTable (stName)

import Data.IntMap as IntMap (fromListWith, lookup)

import Data.List as List (elemIndex)

import Data.Maybe (fromJust)

import Data.Set as Set (elems, fromList, union)


data DependencyRepresentation =
  DependencyRepresentation {
    assumptions :: [Formula String]
  , guarantees :: [Formula String]
  , g2as :: Formula String -> [Formula String]
  }

instance Show DependencyRepresentation where
  show DependencyRepresentation{..} =
    unlines $
      ["assumptions:"]
      ++
      showFormulas assumptions
      ++
      [""]
      ++
      ["guarantees:"]
      ++
      showFormulas guarantees
      ++
      [""]
      ++
      ["dependencies:"]
      ++
      flip map
        (zip [0..] guarantees)
        (\(i,g) ->
          show (i::Int)
          ++
          " <- "
          ++
          show (map aIdx (g2as g))
        )

    where
      showFormulas formulas =
        flip map
          (zip [0..] formulas)
          (\(i,f) -> (show (i::Int)) ++ ": \"" ++ (tslFormula id f) ++ "\"")

      aIdx a = fromJust $ List.elemIndex a assumptions



specifications2dependencies
  :: [Specification] -> DependencyRepresentation

specifications2dependencies specs =
  let
    extractAssumptions :: Specification -> [Formula String]
    extractAssumptions Specification{..} = map (fmap (stName symboltable)) assumptions
    extractGuarantees :: Specification -> [Formula String]
    extractGuarantees Specification{..} = map (fmap (stName symboltable)) guarantees

    assumptions = elems . Set.fromList $ concatMap extractAssumptions specs
    guarantees = elems . Set.fromList $ concatMap extractGuarantees specs

    aIdx a = fromJust $ List.elemIndex a assumptions
    gIdx g = fromJust $ List.elemIndex g guarantees

    gMap =
      IntMap.fromListWith Set.union $
        concatMap (\spec ->
            let aIdxSet = Set.fromList $ map aIdx $ extractAssumptions spec in
            map (\g -> (gIdx g, aIdxSet)) $ extractGuarantees spec
          )
          specs

    g2as g = case IntMap.lookup (gIdx g) gMap of
      Nothing      -> []
      Just aIdxSet -> map (assumptions !!) $ elems aIdxSet

  in
  DependencyRepresentation {..}

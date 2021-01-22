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
  , formulas2dependencies
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



formulas2dependencies
  :: [([Formula String], [Formula String])] -> DependencyRepresentation

formulas2dependencies formulas =
  let
    (assumptionss, guaranteess) = unzip formulas

    assumptions = elems $ Set.fromList $ concat assumptionss
    guarantees = elems $ Set.fromList $ concat guaranteess

    aIdx a = fromJust $ List.elemIndex a assumptions
    gIdx g = fromJust $ List.elemIndex g guarantees

    gMap =
      IntMap.fromListWith Set.union $
        concatMap (\(assumptions, guarantees) ->
            let aIdxSet = Set.fromList $ map aIdx assumptions in
            map (\g -> (gIdx g, aIdxSet)) guarantees
          )
          formulas

    g2as g = case IntMap.lookup (gIdx g) gMap of
      Nothing      -> []
      Just aIdxSet -> map (assumptions !!) $ elems aIdxSet

  in
  DependencyRepresentation {..}


specifications2dependencies
  :: [Specification] -> DependencyRepresentation

specifications2dependencies specs =
  let
    assumptionss = map (extract assumptions) specs
    guaranteess = map (extract guarantees) specs
  in
  formulas2dependencies $ zip assumptionss guaranteess
  where
    extract :: (Specification -> [Formula Int]) -> Specification -> [Formula String]
    extract getFormulas spec@Specification{symboltable} =
      map (fmap (stName symboltable)) $ getFormulas spec

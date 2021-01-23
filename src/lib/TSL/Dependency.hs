-----------------------------------------------------------------------------
-- |
-- Module      :  Dependency
-- Maintainer  :  Marvin Stenger
--
-- This module provides a representation for the dependency relation between
-- a set of guarantees and their assumptions as well as functions to extract
-- this representation out of the given 'Formula's or a given set of
-- 'Specification's.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

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

import Data.Map.Strict as Map (fromListWith, keys, (!))

import Data.List as List (elemIndex)

import Data.Maybe (fromJust)

import Data.Set as Set (elems, fromList, union)

-----------------------------------------------------------------------------

-- | Representation of the dependency relation between
-- a set of guarantees and their assumptions.
data DependencyRepresentation =
  DependencyRepresentation {
    -- | all assumptions in ascending lexical order
    assumptions :: [Formula String]
    -- | all guarantees in ascending lexical order
  , guarantees :: [Formula String]
    -- | mapping from guarantees to their required assumptions
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

-----------------------------------------------------------------------------


-- | Function to extract the dependency relation between
-- a set of guarantees and their assumptions
-- out of a list of 'Formula's (assumptions, guarantees)
formulas2dependencies
  :: [([Formula String], [Formula String])] -> DependencyRepresentation

formulas2dependencies formulas =
  let
    assumptionss = fst <$> formulas
    assumptions = elems $ Set.fromList $ concat assumptionss
    aIdx a = fromJust $ List.elemIndex a assumptions

    gMap =
      Map.fromListWith Set.union $
        concatMap (\(assumptions, guarantees) ->
            let aIdxSet = Set.fromList $ map aIdx assumptions in
            map (,aIdxSet) guarantees
          )
          formulas

    g2as g =
      let aIdxSet = gMap ! g
      in
      map (assumptions !!) $ elems aIdxSet

  in
  DependencyRepresentation
  { guarantees = keys gMap
  , ..
  }

-----------------------------------------------------------------------------

-- | Function to extract the dependency relation between
-- a set of guarantees and their assumptions
-- out of a list of 'Specification's
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

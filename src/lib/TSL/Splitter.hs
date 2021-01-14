-----------------------------------------------------------------------------
-- |
-- Module      :  Splitter
-- Maintainer  :  Gideon Geier
--
-- Splitter which outputs specifications for independant parts of an input
-- specification
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-----------------------------------------------------------------------------

module TSL.Splitter
  ( splitIgnoreAssumptions
  , split
  , splitFormulas
  , makeEdges
  , connectedParts
  , createDepGraph
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable (IdRec(..), Kind(..), SymbolTable(..), symbolTable)

import TSL.Specification (Specification(..))

import TSL.Logic (Formula(..), inputs, outputs)

import Data.Map.Strict as Map
  ( Map
  , delete
  , fromDescList
  , fromListWith
  , keys
  , (!)
  , (!?)
  )

import Data.Set as Set
  ( Set
  , delete
  , difference
  , disjoint
  , empty
  , insert
  , intersection
  , isSubsetOf
  , size
  , toAscList
  , toList
  , union
  , unions
  )

import Data.Array as Ar (listArray, (!))

import Data.Maybe (fromMaybe)

import Data.Ix (range)

-----------------------------------------------------------------------------

-- | Creates separate specifications for independent specification parts

splitIgnoreAssumptions
  :: Specification -> [Specification]
splitIgnoreAssumptions spec =
  let
    guarParts = splitFormulas (guarantees spec) parts
    parts = connectedParts graph
    graph = fromListWith union $ concat outputEdges
    outputEdges = foldl (\ xs x -> makeEdges (outputs x):xs) [] (guarantees spec)
  in
    filterAssumptions . cleanSymboltable <$> buildSpecs guarParts
  where
    buildSpecs = foldl (\ xs x -> spec {guarantees = x} : xs) []


-- | Splits including input dependencies

split
 :: Specification -> [Specification]
split spec =
  let
    graph = createDepGraph spec

    parts = connectedParts graph

    splitGuars = splitFormulas (guarantees spec) parts

    splitInOutputs = zipWith union (map (unions . map getInOutputs) splitGuars) (parts ++ repeat Set.empty)

    splitAssmpts = distributeAssumptions (assumptions spec) splitInOutputs
  in
    fmap cleanSymboltable $ buildSpecs $ zip splitAssmpts splitGuars
  where
    buildSpecs  = foldl (\ xs (a,g) -> spec{assumptions = a, guarantees = g}:xs) []


-- | Creates the dependency graph of the specification
--  It respects impressionable inputs and outputs as nodes

createDepGraph
 :: Specification -> Map.Map Int (Set Int)
createDepGraph spec =
  let
    guaranteeOIIEdges = makeEdgesForNodes  outsAndImpIns (guarantees spec)
    assumptionOIIEdges = makeEdgesForNodes outsAndImpIns (assumptions spec)

    assumptionGraph = createOIGraph $ assumptions spec

    outsAndImpIns = explore allOutputs Set.empty assumptionGraph

    allOutputs = filter (\x -> stKind table x == Output) $ range $ stBounds table

    table = symboltable spec
  in
    fromListWith union $ concat $ guaranteeOIIEdges ++ assumptionOIIEdges
  where
    getOutInsAlsoIn fml nodes = intersection (getInOutputs fml) nodes
    makeEdgesForNodes nodes = foldl (\ xs fml -> makeEdges (getOutInsAlsoIn fml nodes):xs) []


-- | Creates an neighbor relation for a graph from a list of formulas
--  edges are created for:
--   - every pair of inputs appearing in the same formula (bidirectional)
--   - every pair of an out- and an input appearing in the same formula (only out->in)

createOIGraph
 :: [Formula Int] -> Map.Map Int (Set Int)
createOIGraph fmls =
  let
    assumptionIIEdges = foldl (\ xs x -> makeEdges (inputs x):xs) [] fmls
    assumptionOIEdges = foldl (\ xs x -> makeOIEdges (inputs x) (outputs x):xs) [] fmls

  in
    fromListWith union $ concat $ assumptionIIEdges ++ assumptionOIEdges



makeOIEdges :: Set Int -> Set Int -> [(Int, Set Int)]
makeOIEdges inp outp = if size inp < 1 || size outp < 1 then []
                            else foldl (\ xs x -> (x, inp):xs) [] outp

makeEdges :: Set Int -> [(Int, Set Int)]
makeEdges deps = if size deps < 1 then []
                else foldl (\ xs x -> (x, Set.delete x deps):xs) [] deps

-----------------------------------------------------------------------------

-- TODO replace folding over tree by getting (foldr Set.insert Set.empty x) -> (union (inputs x) (outputs x))

-- | Filter Assumptions according to guarantees

filterAssumptions
 :: Specification -> Specification
filterAssumptions spec@Specification{..} =
  let
    filteredAssumptions =
                    filter (\x -> Set.isSubsetOf (foldr Set.insert Set.empty x) vars) assumptions
    vars = foldl (foldr Set.insert) Set.empty guarantees
  in
    spec { assumptions = filteredAssumptions }

-----------------------------------------------------------------------------

-- | Create symboltable for specification part

cleanSymboltable
  :: Specification -> Specification
cleanSymboltable spec@Specification{..} =
  let
    assVars = foldl (foldr Set.insert) Set.empty assumptions
    vars = toAscList $ foldl (foldr Set.insert) assVars guarantees

    newSymbols  = fromDescList mapping
    mapping = snd $ foldl (\ (i, xs) x -> (i+1,(x,i):xs)) (1,[]) vars

    oldNewArr = listArray (1,fst $ head mapping) $ reverse $ fmap fst mapping
    table = fmap (\x -> updateRec (newSymbols Map.!) (symtable symboltable Ar.! x)) oldNewArr
  in
    spec
      { assumptions = fmap (fmap (newSymbols Map.!)) assumptions
      , guarantees  = fmap (fmap (newSymbols Map.!)) guarantees
      , symboltable = symbolTable table
      }

-----------------------------------------------------------------------------

-- | Update the identifiers in one symboltable record
-- TODO update Bindings
updateRec
  :: (Int -> Int) -> IdRec -> IdRec
updateRec dict rec = rec {idArgs = dict <$> idArgs rec, idDeps = dict <$> idDeps rec}

-----------------------------------------------------------------------------

-- | Splits a list of formulas by disjoint sets of variables

splitFormulas
  :: [Formula Int] -> [Set Int] -> [[Formula Int]]
splitFormulas guars parts = map fst guarParts
  where
    guarParts = foldr insertFormula zippedParts guars
    zippedParts = map ([],) parts

insertFormula
 :: Formula Int -> [([Formula Int], Set Int)] -> [([Formula Int], Set Int)]
insertFormula fml   []        = [([fml], empty)]
insertFormula fml ((fs,s):xr) = if not $ disjoint (getInOutputs fml) s
                                -- since s only contains outputs and impressionable inputs,
                                -- checking for disjunctness with all inputs suffices
                                then (fml:fs,s):xr
                                else (fs,s):insertFormula fml xr

-----------------------------------------------------------------------------

-- | Distributes assumptions over sets of in-/ouputs

distributeAssumptions
  :: [Formula Int] -> [Set Int] -> [[Formula Int]]
distributeAssumptions assmpts = map pickAssumptions
  where
    isRelated set fml = isSubsetOf (getInOutputs fml) set
    pickAssumptions set = filter (isRelated set) assmpts

-----------------------------------------------------------------------------

-- | Uses DFS to extract unconnected subgraphs, returns sets of connected nodes

connectedParts
  :: Map.Map Int (Set Int) -> [Set Int]
connectedParts graph = if null graph then [] else parts
    where
    part = explore [head (keys graph)] empty graph
    parts = part:connectedParts (foldr Map.delete graph part)

-----------------------------------------------------------------------------

-- | Does DFS on a Graph represented as a Map and returns the reachable nodes

explore
  :: [Int] -> Set Int -> Map.Map Int (Set Int) -> Set Int
explore []      explored   _    = explored
explore (x:xr)  explored graph  = explore
                        -- add neighbour nodes to queue, but only if not yet explored
                        (toList (difference (fromMaybe Set.empty (graph Map.!? x)) explored) ++ xr)
                        (insert x explored)
                        graph


getInOutputs
  :: Formula Int -> Set Int
getInOutputs fml = inputs fml `union` outputs fml

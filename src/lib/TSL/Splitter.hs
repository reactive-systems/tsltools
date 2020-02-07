-----------------------------------------------------------------------------
-- |
-- Module      :  Splitter
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Splitter which outputs specifications for independant parts of an input
-- specification
--
-----------------------------------------------------------------------------

{-# LANGUAGE
    LambdaCase,
    RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Splitter
  ( split
  , splitWithInputs
  , getInputs
  , splitFormulas
  , makeEdges
  , connectedParts
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable
  ( SymbolTable(..)
  , IdRec(..)
  , stBounds
  , stKind
  , Kind(..)
  )

import TSL.Specification
  ( TSLSpecification(..)
  )

import TSL.Logic
  ( Formula(..)
  )

import TSL.FormulaUtils as Util
  ( getOutputs
  , getInputs
  )

import Data.Map.Strict as Map
  ( Map
  , fromListWith
  , (!)
  , (!?)
  , delete
  , keys
  , fromDescList
  )

import Data.Set as Set
  ( Set
  , empty
  , insert
  , delete
  , size
  , union
  , intersection
  , difference
  , disjoint
  , toList
  , isSubsetOf
  , toAscList
  )

import Data.Array as Ar
  ( listArray
  , (!)
  )

import Data.Maybe
  ( fromMaybe
  )

import Data.Ix
  ( range
  )

import Control.Exception
  ( assert
  )

-- TODO: check whether no outputs were found, in that case output each guarantee as a single specification
-----------------------------------------------------------------------------

-- | Creates separate specifications for independent specification parts

split
  :: TSLSpecification -> [TSLSpecification]
split spec =
  let
    guarParts = splitFormulas (guarantees spec) parts
    parts = connectedParts graph
    graph = fromListWith union $ concat outputEdges
    outputEdges = foldl (\xs -> \x -> makeEdges (getOutputs x):xs) [] (guarantees spec)
  in
    fmap (filterAssumptions . cleanSymboltable) $ buildSpecs guarParts
  where
    buildSpecs = foldl (\xs -> \x -> spec {guarantees = x} : xs) []


-- | Splits including input dependencies

splitWithInputs
 :: TSLSpecification -> [TSLSpecification]
splitWithInputs spec =
  let
    graph = createDepGraph spec

    parts = connectedParts graph

    splitGuars = splitFormulas (guarantees spec) parts
    
    splitAssmpts = splitFormulas (assumptions spec) parts
  in  
    fmap cleanSymboltable $ buildSpecs $ zip splitAssmpts splitGuars
  where
    buildSpecs  = foldl (\xs -> \(a,g) -> spec{assumptions = a, guarantees = g}:xs) [] 


-- | Creates the dependency graph of the specification
--  It respects impressionable inputs and outputs as nodes

createDepGraph
 :: TSLSpecification -> Map.Map Int (Set Int)
createDepGraph spec =
  let  
    guaranteeOIIEdges = makeEdgesForNodes outsAndImpIns (guarantees spec)
    assumptionOIIEdges = makeEdgesForNodes outsAndImpIns (assumptions spec)

    assumptionGraph = createOIGraph $ assumptions spec 
    
    outsAndImpIns = explore allOutputs Set.empty assumptionGraph
    
    allOutputs = filter (\x -> stKind table x == Output) $ range $ stBounds table
    
    table = tslSymboltable spec
  in 
    fromListWith union $ concat $ guaranteeOIIEdges ++ assumptionOIIEdges
  where
    getOutInsAlsoIn fml nodes = (intersection (union (getInputs fml) (getOutputs fml)) nodes)
    makeEdgesForNodes nodes = foldl (\xs -> \fml -> makeEdges (getOutInsAlsoIn fml nodes):xs) []


-- | Creates an neighbor relation for a graph from a list of formulas
--  edges are created for:
--   - every pair of inputs appearing in the same formula (bidirectional)
--   - every pair of an out- and an input appearing in the same formula (only out->in)
   
createOIGraph
 :: [Formula Int] -> Map.Map Int (Set Int)
createOIGraph fmls =
  let
    assumptionIIEdges = foldl (\xs -> \x -> makeEdges (getInputs x):xs) [] fmls
    assumptionOIEdges = foldl (\xs -> \x -> makeOIEdges (getInputs x) (getOutputs x):xs) [] fmls

  in
    fromListWith union $ concat $ assumptionIIEdges ++ assumptionOIEdges



makeOIEdges :: Set Int -> Set Int -> [(Int, Set Int)]
makeOIEdges inputs outputs = if size inputs < 1 || size outputs < 1 then []
                            else foldl (\xs -> \x -> (x, inputs):xs) [] outputs

makeEdges :: Set Int -> [(Int, Set Int)]
makeEdges deps = if size deps < 1 then []
                else foldl (\xs -> \x -> (x, Set.delete x deps):xs) [] deps

-----------------------------------------------------------------------------

-- TODO replace folding over tree by getting (foldr Set.insert Set.empty x) -> (union (getInputs x) (getOutputs x))

-- | Filter Assumptions according to guarantees

filterAssumptions
 :: TSLSpecification -> TSLSpecification
filterAssumptions TSLSpecification{..} = 
  let
    filteredAssumptions = 
                    filter (\x -> Set.isSubsetOf ((foldr Set.insert) Set.empty x) vars) assumptions
    vars = foldl (foldr Set.insert) Set.empty guarantees 
  in
    TSLSpecification { guarantees     = guarantees
                     , assumptions    = filteredAssumptions
                     , tslSymboltable = tslSymboltable
                     }
    
-----------------------------------------------------------------------------

-- | Create symboltable for specification part

cleanSymboltable
  :: TSLSpecification -> TSLSpecification
cleanSymboltable TSLSpecification{..} =
  let
    assVars = (foldl (foldr Set.insert) Set.empty assumptions)
    vars = toAscList $ foldl (foldr Set.insert) assVars guarantees 

    newSymbols  = fromDescList $ mapping
    mapping = snd $ foldl (\(i, xs) -> \x -> (i+1,(x,i):xs)) (1,[]) vars
    
    oldNewArr = listArray (1,fst $ head mapping) $ reverse $ fmap fst mapping
    table = fmap (\x -> updateRec ((Map.!) newSymbols) ((symtable tslSymboltable) Ar.! x)) oldNewArr
  in
    TSLSpecification { assumptions    = fmap (fmap ((Map.!) newSymbols)) assumptions
                     , guarantees     = fmap (fmap ((Map.!) newSymbols)) guarantees
                     , tslSymboltable = SymbolTable{symtable=table}
                     }


-----------------------------------------------------------------------------

-- | Update the identifiers in one symboltable record 
-- TODO update Bindings
updateRec
  :: (Int -> Int) -> IdRec -> IdRec
updateRec dict rec = rec {idArgs = fmap dict $ idArgs rec, idDeps = fmap dict $ idDeps rec}

-----------------------------------------------------------------------------

-- | Splits a list of formulas by disjoint sets of variables 

splitFormulas
  :: [Formula Int] -> [Set Int] -> [[Formula Int]]
splitFormulas guars parts = map fst guarParts
  where
    guarParts = foldr insertFormula zippedParts guars
    zippedParts = foldl (\xs -> \s -> ([],s):xs) [] parts

insertFormula
 :: Formula Int -> [([Formula Int], Set Int)] -> [([Formula Int], Set Int)]
--                              Assertion: invariant does not permit this case
insertFormula _   []          = assert False undefined 
insertFormula fml [(fs,s)]    = [(fml:fs,s)]
insertFormula fml ((fs,s):xr) = if not $ disjoint (foldr Set.insert Set.empty fml) s
                                then (fml:fs,s):xr
                                else (fs,s):insertFormula fml xr

-----------------------------------------------------------------------------

-- | Uses DFS to extract unconnected subgraphs, returns sets of connected nodes

connectedParts
  :: Map.Map Int (Set Int) -> [Set Int]
connectedParts graph = if null graph then [] else parts
    where
    part = (explore [head (keys graph)] empty graph)
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

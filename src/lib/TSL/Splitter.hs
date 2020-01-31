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
  , fromList
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
-----------------------------------------------------------------------------

-- | Creates separate specifications for independent specification parts

split
  :: TSLSpecification -> [TSLSpecification]

split spec = fmap fixSymboltable $ fmap filterAssumptions $ foldl (\xs -> \x -> spec{guarantees = x}:xs) [] guarParts
  where
    
    guarParts = splitFormulas (guarantees spec) parts
    -- TODO: check whether no outputs were found, in that case output each guarantee as a single specification
    parts = connectedParts graph

    graph = fromListWith union $ concat $ foldl (\xs -> \x -> makeEdges (getOutputs x):xs) [] (guarantees spec)

-- | Splits including input dependencies

-- TODO replace folding over tree by getting (foldr Set.insert Set.empty x) -> (union (getInputs x) (getOutputs x))

splitWithInputs
 :: TSLSpecification -> [TSLSpecification]
splitWithInputs spec = fmap fixSymboltable $ foldl (\xs -> \(a,g) -> spec{assumptions = a, guarantees = g}:xs) [] $ zip splitAssmpts splitGuars
  where
    graph = fromListWith union $ guaranteeOIIEdges ++ assumptionOIIEdges 
    
    guaranteeOIIEdges = concat $ foldl (\xs -> \x -> makeEdges (intersection (union (getInputs x) (getOutputs x)) outsAndImpIns):xs) [] (guarantees spec)
    assumptionOIIEdges = concat $ foldl (\xs -> \x -> makeEdges (intersection (union (getInputs x) (getOutputs x)) outsAndImpIns):xs) [] (assumptions spec)

    assumptionIIEdges= concat $ foldl (\xs -> \x -> makeEdges (Util.getInputs x):xs) [] (assumptions spec)
    assumptionOIEdges = concat $ foldl (\xs -> \x -> makeIOEdges (Util.getInputs x) (getOutputs x):xs) [] (assumptions spec)

    assumptionGraph = fromListWith union $ assumptionIIEdges ++ assumptionOIEdges
    
    outsAndImpIns = explore allOutputs Set.empty assumptionGraph
    
    table = tslSymboltable spec

    allOutputs = filter (\x -> stKind table x == Output) $ range $ stBounds table
    
    allInputs = filter (\x -> stKind table x == Input) $ range $ stBounds table

    parts = connectedParts graph

    splitGuars = splitFormulas (guarantees spec) parts
    
    splitAssmpts = splitFormulas (assumptions spec) parts


makeIOEdges :: Set Int -> Set Int -> [(Int, Set Int)]
makeIOEdges inputs outputs = if size inputs < 1 || size outputs < 1 then [] else foldl (\xs -> \x -> (x, inputs):xs) [] outputs

makeEdges :: Set Int -> [(Int, Set Int)]
makeEdges deps = if size deps < 1 then [] else foldl (\xs -> \x -> (x, Set.delete x deps):xs) [] deps

-----------------------------------------------------------------------------

-- | Filter Assumptions according to guarantees

filterAssumptions
 :: TSLSpecification -> TSLSpecification
filterAssumptions TSLSpecification{..} = TSLSpecification{guarantees = guarantees, assumptions = filteredAssumptions, tslSymboltable = tslSymboltable}
  where
    filteredAssumptions = filter (\x -> Set.isSubsetOf ((foldr Set.insert) Set.empty x) vars) assumptions
    vars = foldl (foldr Set.insert) Set.empty guarantees 
    
    
-----------------------------------------------------------------------------

-- | Create symboltable for specification part

fixSymboltable
  :: TSLSpecification -> TSLSpecification
fixSymboltable TSLSpecification{..} = TSLSpecification {assumptions = fmap (fmap ((Map.!) newSymbols)) assumptions, guarantees = fmap (fmap ((Map.!) newSymbols)) guarantees, tslSymboltable = SymbolTable{symtable=table}} 
  where
    assVars = (foldl (foldr Set.insert) Set.empty assumptions)
    vars = toAscList $ foldl (foldr Set.insert) assVars guarantees -- Baumfaltung ftw
    newSymbols  = fromDescList $ mapping
    mapping = snd $ foldl (\(i, xs) -> \x -> (i+1,(x,i):xs)) (1,[]) vars
    oldNewArr = listArray (1,fst $ head mapping) $ reverse $ fmap fst mapping
    table = fmap (\x -> updateRec ((Map.!) newSymbols) ((symtable tslSymboltable) Ar.! x)) oldNewArr

-----------------------------------------------------------------------------

-- | Update the identifiers in one symboltable record 
-- TODO update Bindings
updateRec
  :: (Int -> Int) -> IdRec -> IdRec
updateRec dict rec = rec{idArgs = fmap dict $ idArgs rec, idDeps = fmap dict $ idDeps rec}

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
insertFormula _   []          = error "Assertion: invariant does not permit this case"
insertFormula fml [(fs,s)]    = [(fml:fs,s)]
insertFormula fml ((fs,s):xr) = if not $ disjoint (foldr Set.insert Set.empty fml) s then (fml:fs,s):xr else (fs,s):insertFormula fml xr

-----------------------------------------------------------------------------

-- | Uses exploration to extract unconnected parts from the graph, returns sets of connected nodes

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
explore (x:xr)  explored graph  = explore (toList (difference (fromMaybe Set.empty (graph Map.!? x)) explored) ++ xr)
                                            (insert x explored) graph
    


-----------------------------------------------------------------------------

-- | Extracts the outputs from one formula

dependents
  :: Formula Int -> Set Int
dependents fml = outs empty fml
    where
    outs set  = \case
      TTrue       -> set
      FFalse      -> set
      Check {}    -> set 
      Update a _  -> insert a set 
      Not x       -> outs set x
      Implies x y -> outs (outs set x) y
      Equiv x y   -> outs (outs set x) y
      And xs      -> foldl outs set xs
      Or xs       -> foldl outs set xs
      Next x      -> outs set x
      Globally x  -> outs set x
      Finally x   -> outs set x
      Until x y   -> outs (outs set x) y
      Release x y -> outs (outs set x) y
      Weak x y    -> outs (outs set x) y


-----------------------------------------------------------------------------
{-
-- | Extracts the outputs from one formula

getInputs
 :: SymbolTable -> Set Int
getInputs table = inputs
  where
    (start, end) = stBounds table
    inputs = fromList $ filter (\x -> stKind table x == Input ) [start..end]
-}
-----------------------------------------------------------------------------

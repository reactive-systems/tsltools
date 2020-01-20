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
  , makeNodes
  , dependents
  , filterAssumptions
  , fixSymboltable
  , splitGuarantees
  , insertGuarantee
  , connectedParts
  , explore
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable
  ( SymbolTable(..)
  , IdRec(..)
  )

import TSL.Specification
  ( TSLSpecification(..)
  )

import TSL.Logic
  ( Formula(..)
  )

import Data.Map.Strict as Map
  ( Map
  , fromListWith
  , (!)
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
  , difference
  , toList
  , isSubsetOf
  , toAscList
  )

import Data.Array as Ar
  ( listArray
  , (!)
  )
-----------------------------------------------------------------------------

-- | Creates separate specifications for independent specification parts

split
  :: TSLSpecification -> [TSLSpecification]

split spec = fmap fixSymboltable $ fmap filterAssumptions $ foldl (\xs -> \x -> spec{guarantees = x}:xs) [] guarParts
  where
    
    guarParts = splitGuarantees (guarantees spec) parts

    parts = connectedParts graph

    graph = fromListWith union $ concat $ foldl (\xs -> \x -> makeNodes (dependents x):xs) [] (guarantees spec)
 
makeNodes :: Set Int -> [(Int, Set Int)]
makeNodes deps = if size deps < 1 then [] else foldl (\xs -> \x -> (x, Set.delete x deps):xs) [] deps

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

-- | 

splitGuarantees
  :: [Formula Int] -> [Set Int] -> [[Formula Int]]
splitGuarantees guars parts = map fst guarParts
  where
    guarParts = foldr insertGuarantee zippedParts guars
    zippedParts = foldl (\xs -> \s -> ([],s):xs) [] parts

insertGuarantee
 :: Formula Int -> [([Formula Int], Set Int)] -> [([Formula Int], Set Int)]
insertGuarantee _   []          = error "Assertion: invariant does not permit this case"
insertGuarantee fml [(fs,s)]    = [(fml:fs,s)]
insertGuarantee fml ((fs,s):xr) = if isSubsetOf (dependents fml) s then (fml:fs,s):xr else (fs,s):insertGuarantee fml xr
-- use of dependents inefficient

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
explore (x:xr)  explored graph  = explore (toList (difference (graph Map.! x) explored) ++ xr)
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

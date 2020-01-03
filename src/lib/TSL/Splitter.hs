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
  ( --split
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable
  ( SymbolTable(..)
  , IdRec(..)
  , Kind(..)
  )

import TSL.Specification
  ( TSLSpecification(..)
  )

import TSL.Logic
  ( Formula(..)
  , SignalTerm(..)
  , tlsfFormula
  , tlsfPredicates
  , tlsfUpdates
  , exactlyOne
  )

import Data.Map.Strict as Map
  ( Map
  , fromListWith
  , (!)
  , delete
  , keys
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
  )

import Data.Array
  ( Array
  , elems
  , assocs
  )
-----------------------------------------------------------------------------

-- | Creates separate specifications for independant specification parts

split
  :: TSLSpecification -> [TSLSpecification]

split TSLSpecification{..} = []
  where
    
    guarParts = splitGuarantees guarantees parts

    parts = connectedParts graph

    graph = fromListWith union $ concat $ foldl (\xs -> \x -> makeNodes (dependents x):xs) []  guarantees



--    inputs = foldl (\xs -> \(i, x) -> if idKind x == Input then i:xs else xs) [] $ assocs $ symtable tslSymboltable

--    outputs = foldl (\xs -> \(i, x) -> if idKind x == Output then i:xs else xs) [] $ assocs $ symtable tslSymboltable

    makeNodes :: Set Int -> [(Int, Set Int)]
    makeNodes deps = if size deps <= 1 then [] else foldl (\xs -> \x -> (x, Set.delete x deps):xs) [] deps

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
explore []      explored graph  = explored
explore (x:xr)  explored graph  = explore (toList (difference (graph ! x) explored) ++ xr)
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

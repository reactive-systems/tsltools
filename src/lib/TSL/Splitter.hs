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

import Data.Map.Strict 
  ( Map
  , fromListWith
  , (!)
  , delete as mapDelete
  )

import Data.Set
  ( Set
  , empty
  , insert
  , delete
  , size
  , union
  , difference
  , toList
  )

import Data.Array
  ( Array
  , elems
  )
-----------------------------------------------------------------------------

-- | Creates separate specifications for independant specification parts

split
  :: TSLSpecification -> [TSLSpecification]

split TSLSpecification{..} = []
  where
    



    graph = fromListWith union $ concat $ foldl (\xs -> \x -> makeNodes (dependents x):xs) []  guarantees



    inputs = foldl (\xs -> \x -> if idKind x == Input then x:xs else xs) [] $ symtable tslSymboltable

    outputs = foldl (\xs -> \x -> if idKind x == Input then x:xs else xs) [] $ symtable tslSymboltable
    makeNodes :: Set Int -> [(Int, Set Int)]
    makeNodes deps = if size deps <= 1 then [] else foldl (\xs -> \x -> (x, delete x deps):xs) [] deps

-----------------------------------------------------------------------------

-- | 

connectedParts
  :: Map Int (Set Int) -> [Set Int]
connectedParts graph = if null graph then [] else parts
    where
    part = (explore [hd keys graph] empty graph)
    parts = part:connectedParts (foldr mapDelete graph part)

-----------------------------------------------------------------------------

-- | Does DFS on a Graph represented as a Map and returns the reachable states

explore
  :: [Int] -> Set Int -> Map Int (Set Int) -> Set Int
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

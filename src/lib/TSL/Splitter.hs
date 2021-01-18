-----------------------------------------------------------------------------
-- |
-- Module      :  Splitter
-- Maintainer  :  Gideon Geier
--
-- 'Splitter' implements algorithms to split TSL specifications into
-- independent subspecifications.
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-----------------------------------------------------------------------------

module TSL.Splitter
  ( split
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable (IdRec(..), Kind(..), SymbolTable(..), symbolTable)

import TSL.Specification (Specification(..))

import TSL.Logic (Formula(..), inputs, outputs)

import Data.Map.Strict as Map (fromDescList, (!))

import Data.Set as Set
  ( Set
  , disjoint
  , elems
  , empty
  , fromList
  , insert
  , intersection
  , toAscList
  , union
  )

import Data.List (partition)

import Data.Array as Ar (listArray, (!))

import Data.Ix (range)

import Data.Graph.Inductive.Graph (LEdge, Node, mkGraph)

import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Query.DFS

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



getInOutputs
  :: Formula Int -> Set Int
getInOutputs fml = inputs fml `union` outputs fml

-----------------------------------------------------------------------------

-- | 'split' implements an optimized decomposition algorithm which analyses
-- assumptions and guarantees. This algorithm preserves equisynthesizeability
-- as long as for a given specification [assumptions] -> [guarantees]
-- [assumptions] -> bot is unrealizable.

split :: Specification -> [Specification]
split spec =
  let
    ass = assumptions spec
    guar = guarantees spec
    decRelProps = decompRelProps spec
    graph = buildGraph (elems decRelProps) preEdges
    preEdges = map (elems . (intersection decRelProps . getInOutputs)) (ass ++ guar)
    connComp = map fromList $ components graph
    (freeAss, boundAss) = partition (null . (intersection decRelProps . getInOutputs)) ass
    splitGuars = splitFormulas guar connComp
    splitAss  = splitFormulas boundAss connComp
  in
    fmap cleanSymboltable $ buildSpecs $ addFreeAssumptions freeAss $ zip splitAss splitGuars
  where
    buildSpecs  = foldl (\ xs (a,g) -> spec{assumptions = a, guarantees = g}:xs) []


-----------------------------------------------------------------------------

-- | 'addFreeAssumptions' requires a list of input-only assumptions and a list
-- of assumption - guarantee specification pairs. It then adds the assumptions
-- such that [not added] and [added], [assumptions], [guarantees] do not share
-- any propositions.

addFreeAssumptions :: [Formula Int] -> [([Formula Int], [Formula Int])] -> [([Formula Int], [Formula Int])]
addFreeAssumptions freeAss specs =
  let
    graph = buildGraph (elems (foldr union empty inpAss)) preEdges
    preEdges = map elems inpAss
    inpAss = map inputs freeAss
    assParts = map fromList $ components graph
    splitFreeAss = splitFormulas freeAss assParts
  in
    map (addFreeAssumpt splitFreeAss) specs
  where
    addFreeAssumpt :: [[Formula Int]] -> ([Formula Int], [Formula Int]) -> ([Formula Int], [Formula Int])
    addFreeAssumpt freeAssParts (assmpts, guars) =
      let
        props = foldr (union . getInOutputs) empty (assmpts ++ guars)
        matchAssmpt = concat $ filter (not . disjoint props . foldr (union . getInOutputs) empty) freeAssParts
      in
        (matchAssmpt ++ assmpts, guars)

-----------------------------------------------------------------------------

-- | 'decompRelProps' implements an algorithm to find propositions relevant for
-- decomposition. Those are all outputs and all inputs that can be influenced
-- by outputs.

decompRelProps :: Specification -> Set Int
decompRelProps spec =
  let
    (out, inp) = partition (\x -> stKind table x == Output) $ range $ stBounds table
    table = symboltable spec
    graph = buildGraph (out++inp) preEdges
    preEdges = map (elems . getInOutputs) (assumptions spec)

  in
    Set.fromList $ udfs out graph

-----------------------------------------------------------------------------

-- | 'buildGraph' builds a graph with all elements of intNodes as nodes and
-- guarantees nodes in the same list in intOutputs to be in the same connected
-- component. E.g. buildGraph [1,2,3,4,5,6] [[1,2,3],[3,5],[4,6]]
-- will result in a graph that has six nodes and the connected components
-- [1,2,3,5] and [4,6].

buildGraph :: [Int] -> [[Int]] -> Gr () ()
buildGraph intNodes intOutputs =
        mkGraph (map intToNode intNodes) (concatMap buildEdges intOutputs)
    where
        intToNode n = (n, ())

        buildEdges :: [Node] -> [LEdge ()]
        buildEdges []     = []
        buildEdges (x:xr) = map (tupToEdge x) xr -- ++ buildEdges xr
        tupToEdge :: Node -> Node -> LEdge ()
        tupToEdge a b = (a,b,())

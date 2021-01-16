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
  ( split
  , splitIgnoreAssumptions
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
  , isSubsetOf
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

-- | Creates separate specifications for independent specification parts

splitIgnoreAssumptions
  :: Specification -> [Specification]
splitIgnoreAssumptions spec =
  let
    guarParts = splitFormulas (guarantees spec) parts
    graph = buildGraph out preEdges
    parts = map fromList $ components graph
    preEdges = map (elems . outputs) (guarantees spec)
    out = filter (\x -> stKind table x == Output) $ range $ stBounds table
    table = symboltable spec
  in
    filterAssumptions . cleanSymboltable <$> buildSpecs guarParts
  where
    buildSpecs = foldl (\ xs x -> spec {guarantees = x} : xs) []


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



getInOutputs
  :: Formula Int -> Set Int
getInOutputs fml = inputs fml `union` outputs fml


-- | Splits including input dependencies
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

decompRelProps :: Specification -> Set Int
decompRelProps spec =
  let
    (out, inp) = partition (\x -> stKind table x == Output) $ range $ stBounds table
    table = symboltable spec
    graph = buildGraph (out++inp) preEdges
    preEdges = map (elems . getInOutputs) (assumptions spec)

  in
    Set.fromList $ udfs out graph

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

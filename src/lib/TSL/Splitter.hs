-----------------------------------------------------------------------------
-- |
-- Module      :  Splitter
-- Maintainer  :  Gideon Geier
--
-- 'Splitter' implements algorithms to split TSL specifications into
-- independent subspecifications.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-----------------------------------------------------------------------------

module TSL.Splitter
  ( split
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable (IdRec(..), Kind(..), SymbolTable(..), symbolTable)

import TSL.Specification (Specification(..))

import TSL.Logic (Formula(..), inputs, outputs, symbols)

import Data.Maybe (fromJust)

import Data.Set as Set
  ( Set
  , disjoint
  , elems
  , empty
  , fromList
  , intersection
  , union
  , unions
  )

import Data.List (elemIndex, partition)

import Data.Array as Array (listArray, (!))

import Data.Ix (range)

import Data.Graph.Inductive.Graph (LEdge, Node, mkGraph)

import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Query.DFS

-----------------------------------------------------------------------------


getInOutputs :: Formula Int -> Set Int
getInOutputs fml = inputs fml `union` outputs fml

-----------------------------------------------------------------------------
-- | Create symboltable for specification part
cleanSymboltable :: Specification -> Specification
cleanSymboltable spec@Specification{..} =
  let
    vars = elems . Set.unions $ map symbols (assumptions ++ guarantees)

    -- mapping from old to new variables ((index in vars) + 1)
    old2new :: Int -> Int
    old2new = (+1) . fromJust . (`elemIndex` vars)

    records = map (\x -> updateRec old2new (symtable symboltable ! x)) vars
    table = list2array records
  in
    spec
      { assumptions = map (fmap old2new) assumptions
      , guarantees  = map (fmap old2new) guarantees
      , symboltable = symbolTable table
      }
  where
    list2array l = Array.listArray (1, length l) l

-----------------------------------------------------------------------------

-- | Update the identifiers in one symboltable record
updateRec :: (Int -> Int) -> IdRec -> IdRec
updateRec dict rec@IdRec{idArgs, idDeps, idBindings} =
  rec
  { idArgs = dict <$> idArgs
  , idDeps = dict <$> idDeps
  , idBindings = (dict <$>) <$> idBindings
  }

-----------------------------------------------------------------------------

-- | Splits a list of formulas by disjoint sets of variables
splitFormulas :: [Formula Int] -> [Set Int] -> [[Formula Int]]
splitFormulas formulas parts = map fst formulaParts
  where
    formulaParts = foldr insertFormula zippedParts formulas
    zippedParts = map ([],) parts


insertFormula :: Formula Int -> [([Formula Int], Set Int)] -> [([Formula Int], Set Int)]
insertFormula formula   []                        = [([formula], empty)]
insertFormula formula ((formulas,variableSet):xr)
                          = if not $ disjoint (getInOutputs formula) variableSet
                            then (formula:formulas,variableSet):xr
                            else (formulas,variableSet):insertFormula formula xr


-----------------------------------------------------------------------------

-- | 'split' implements an optimized decomposition algorithm which analyses
-- assumptions and guarantees. This algorithm preserves equisynthesizeability
-- as long as for a given specification [assumptions] -> [guarantees]
-- [assumptions] -> bot is unrealizable.

split :: Specification -> [Specification]
split spec@Specification{assumptions, guarantees} =
  let
    decRelProps = decompRelProps spec
    graph = buildGraph (elems decRelProps) preEdges
    preEdges = map (elems . (intersection decRelProps) . getInOutputs) (assumptions ++ guarantees)
    connComp = map Set.fromList $ components graph
    (freeAssumptions, boundAssumptions) = partition (null . (intersection decRelProps) . getInOutputs) assumptions
    splitGuars = splitFormulas guarantees connComp
    splitAssumptions  = splitFormulas boundAssumptions connComp
  in
    fmap cleanSymboltable $ buildSpecs $ addFreeAssumptions freeAssumptions $ filter (not . null . snd) $ zip (splitAssumptions ++ repeat []) splitGuars
  where
    buildSpecs  = map (\(a,g) -> spec{assumptions = a, guarantees = g})


-----------------------------------------------------------------------------

-- | 'addFreeAssumptions' requires a list of input-only assumptions and a list
-- of assumption - guarantee specification pairs. It then adds the assumptions
-- such that [not added] and [added], [assumptions], [guarantees] do not share
-- any propositions.

addFreeAssumptions :: [Formula Int] -> [([Formula Int], [Formula Int])] -> [([Formula Int], [Formula Int])]
addFreeAssumptions freeAssumptions specs =
  let
    assumptionsInputs = map inputs freeAssumptions
    preEdges = map elems assumptionsInputs
    graph = buildGraph (elems (Set.unions assumptionsInputs)) preEdges
    assParts = Set.fromList <$> components graph
    freeAssumptionsSplit = splitFormulas freeAssumptions assParts
  in
    map (addFreeAssumpt freeAssumptionsSplit) specs
  where
    addFreeAssumpt :: [[Formula Int]] -> ([Formula Int], [Formula Int]) -> ([Formula Int], [Formula Int])
    addFreeAssumpt freeAssumptionParts (assmpts, guars) =
      let
        props = Set.unions $ map getInOutputs (assmpts ++ guars)
        matchAssmpt = concat $ filter (not . disjoint props . Set.unions . (map getInOutputs)) freeAssumptionParts
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

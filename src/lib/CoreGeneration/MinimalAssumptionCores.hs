-------------------------------------------------------------------------------
-- |
-- Module      : MinimalAssumptionCores
-- Description : TSL minimal-assumptions finder
-- Maintainer  : Philippe Heim
--
-- This module provides methods to search for a minimal amount of assumptions
-- needed to realize a specification.
--
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-------------------------------------------------------------------------------
module CoreGeneration.MinimalAssumptionCores
  ( generateMinimalAssumptions
  , treeBasedMinimalAssumptions
  ) where

-------------------------------------------------------------------------------
import Data.List as List (splitAt)

import CoreGeneration.CoreUtilities
  ( Context(..)
  , logHigh
  , logNormal
  , optimizeSpec
  , sortedPowerSet
  )

import Data.Set as Set (Set, difference, empty, fromList, member, toList, union)

import TSL (Formula(..), Specification(..), SymbolTable, split, toTSL)

import CoreGeneration.FindFirstConcurrent (incParallelFirst)

-------------------------------------------------------------------------------
-- | 'generateMinimalAssumptions' searches for a 'Specification' a minimal
-- sub-specification (w.r.t. the assuptions) that is realizable (a so-called
-- assumption core) if it exists. Since this is done using synthesis queries,
-- a 'Context' is needed.
generateMinimalAssumptions ::
     Context -> Specification -> IO (Maybe Specification)
generateMinimalAssumptions context tsl =
  let assmpt = assumptions tsl
      indicesList = sortedPowerSet $ length assmpt
      potQueries =
        fmap
          (\indices ->
             ( (tsl
                  { assumptions =
                      fmap snd $
                      Prelude.filter (\(a, _) -> member a indices) $
                      zip [0 ..] assmpt
                  })
             , length indices))
          indicesList
      queries = fst <$> Prelude.filter (uncurry splitedAssumptions) potQueries
   in incParallelFirst (threadPoolSize context) (fmap test queries)
  where
    test :: Specification -> IO (Maybe Specification)
    test spec = do
      logNormal context $
        "Test query with " ++ show (length (assumptions spec)) ++ " assumptions"
      logHigh context $ "Query:\n" ++ toTSL (optimizeSpec spec) ++ "\n"
      rel <- tslSpecRealizable context (optimizeSpec spec)
      if rel
        then return $ Just spec
        else return Nothing
    -- Not already seen
    splitedAssumptions spec num =
      case split spec of
        [s] -> length (assumptions s) == num
        _   -> False

-------------------------------------------------------------------------------
-- | 'AssumptionTree' is a binary tree which representing which assumptions are
-- needed for some combination of guarantees. The guarantees are in the leafs
-- of the tree. The nodes are annotated with assumptions representing that its
-- children need these assumptions to be realizable together.
data AssumptionTree
  -- | A 'Leaf' of an 'AssumptionTree' holds a guarantee and a set
  -- of assumptions (which are of type 'Formula' 'Int') each.
  = Leaf (Set (Formula Int)) (Formula Int)
  -- | A (inner) 'Node' of an 'AssumptionTree' holds the set of
  -- assumptions that allows the conjunction of both sub-trees
  -- to by synthesized.
  | Node (Set (Formula Int)) AssumptionTree AssumptionTree

-------------------------------------------------------------------------------
-- | 'liftAssumption' aims to reduce the amount of assumptions in an
-- 'AssumptionTree' without changing the realizability by lifting the common
-- assumptions of two children to their common parent.
liftAssumptions :: AssumptionTree -> AssumptionTree
liftAssumptions =
  \case
    Node assmpt t1 t2 ->
      let t1p = liftAssumptions t1
          t2p = liftAssumptions t2
          commonDirectSubAssumptions =
            union (directSubAssumptions t1p) (directSubAssumptions t2p)
       in Node
            (assmpt `union` commonDirectSubAssumptions)
            (removeDirectSubAssumption commonDirectSubAssumptions t1p)
            (removeDirectSubAssumption commonDirectSubAssumptions t2p)
    leaf -> leaf
  where
    directSubAssumptions =
      \case
        Leaf asmpt _   -> asmpt
        Node asmpt _ _ -> asmpt
    removeDirectSubAssumption rmAsmpt =
      \case
        Leaf asmpt g     -> Leaf (difference asmpt rmAsmpt) g
        Node asmpt t1 t2 -> Node (difference asmpt rmAsmpt) t1 t2

-------------------------------------------------------------------------------
-- | 'cleanAssumptions' aims to reduce the amount of assumptions in an
-- 'AssumptionTree' without changing the realizability by removing (redundant)
-- assumptions that are already required in the parent.
cleanAssumptions :: AssumptionTree -> AssumptionTree
cleanAssumptions = help empty
  where
    help :: Set (Formula Int) -> AssumptionTree -> AssumptionTree
    help known =
      \case
        Leaf asmpt g -> Leaf (difference asmpt known) g
        Node asmpt t1 t2 ->
          let newKnown = union asmpt known
           in Node
                (difference asmpt known)
                (help newKnown t1)
                (help newKnown t2)

-------------------------------------------------------------------------------
-- | 'reduce' removes unnecessary assumptions of an 'AssumptionTree' by first
-- lifting assumptions (using 'liftAssumpions') and then cleaning up
-- (using 'cleanAssumptions'). This functions should be used when reducing
-- assumption trees over the individual optimizations
reduce :: AssumptionTree -> AssumptionTree
reduce = cleanAssumptions . liftAssumptions

-------------------------------------------------------------------------------
-- | 'flatten' converts an 'AssumptionTree' into a conjunction of formulas
-- (represented as list of 'Formula' 'Int') which are equivalent to
-- the formula represented by the 'AssumptionTree'. Therefore, the
-- distributivity law of the implication is applied through the tree.
flatten :: AssumptionTree -> [Formula Int]
flatten =
  \case
    Leaf asmpt g     -> [applyAssumptions asmpt g]
    Node asmpt t1 t2 -> fmap (applyAssumptions asmpt) (flatten t1 ++ flatten t2)
  where
    applyAssumptions asmpt formula =
      case toList asmpt of
        [] -> formula
        xs -> Or $ fmap Not xs ++ [formula]

-------------------------------------------------------------------------------
-- | 'assumptionTreeToSpec' generates a 'Specification' out of an
-- 'AssumtptionTree', representing the guarantees, a list of 'Formula Int'
-- representing additional assumptions and a respective 'SymbolTable'.
assumptionTreeToSpec ::
     SymbolTable -> [Formula Int] -> AssumptionTree -> Specification
assumptionTreeToSpec sym asmpt atree =
  Specification
    { assumptions = asmpt
    , guarantees = flatten (reduce atree)
    , symboltable = sym
    }

-------------------------------------------------------------------------------
-- | 'SearchTree' represents a (generic) binary search tree, where the
-- searched objects are at the leafs.
data SearchTree a
  = STLeaf a
  | STNode (SearchTree a) (SearchTree a)

-----------------------------------------------------------------------------
-- | 'searchTree' generates a 'SearchTree' out of a list where the elements
-- of the list are at the leaf of the 'SearchTree'. Note that 'searchTree'
-- aims for a balanced search tree.
searchTree :: [a] -> Maybe (SearchTree a)
searchTree =
  \case
    [] -> Nothing
    [x] -> Just (STLeaf x)
    xs ->
      let (a, b) = splitAt (length xs `div` 2) xs
       in case (searchTree a, searchTree b) of
            (Nothing, Nothing) -> Nothing
            (Just s, Nothing)  -> Just s
            (Nothing, Just s)  -> Just s
            (Just s, Just t)   -> Just (STNode s t)

-----------------------------------------------------------------------------
-- | 'minimalAssumptionTree' searches for an 'AssuptionTree' with a minimal
-- amount of assumptions provided a 'SearchTree' defining the order the
-- guarantees  should be searched through. Note that for a specific node the
-- 'AssumptionTree' is converted into a 'Specification' and the minimal amount
-- of assumptions is searched using 'generateMinimalAssumptions'.
minimalAssumptionTree ::
     Context
  -> [Formula Int]
  -> SymbolTable
  -> SearchTree (Formula Int)
  -> IO (Maybe AssumptionTree)
minimalAssumptionTree context asmpt sym =
  \case
    STLeaf g -> do
      logNormal context "Start searching leaf"
      potSpec <-
        subGeneration
          (Specification
             {symboltable = sym, guarantees = [g], assumptions = asmpt})
      logNormal context "Finished searching leaf"
      case potSpec of
        Nothing   -> return Nothing
        Just spec -> return $ Just $ Leaf (fromList $ assumptions spec) g
    STNode t1 t2 -> do
      a1p <- minimalAssumptionTree context asmpt sym t1
      a2p <- minimalAssumptionTree context asmpt sym t2
      case (a1p, a2p) of
        (Just a1, Just a2) -> do
          let spec1 = assumptionTreeToSpec sym [] a1
          let spec2 = assumptionTreeToSpec sym [] a2
          logNormal context "Start searching inner node"
          potSpec <-
            subGeneration
              (Specification
                 { symboltable = sym
                 , guarantees = guarantees spec1 ++ guarantees spec2
                 , assumptions = asmpt
                 })
          logNormal context "Finished searching inner node"
          case potSpec of
            Nothing -> return Nothing
            Just spec ->
              return $ Just $ Node (fromList $ assumptions spec) a1 a2
        _ -> return Nothing
  where
    subGeneration = generateMinimalAssumptions context

-------------------------------------------------------------------------------
-- | Similar to 'generateMinimalAssumptions', 'treeBasedMinimalAssumptions'
-- searches for a 'Specification' a minimal sub-specification (w.r.t. the
-- assumptions) that is realizable (a so-called assumption core) if it exists.
-- The main difference it that is uses a 'AssumptionTree' to perform this
-- search.
treeBasedMinimalAssumptions ::
     Context -> Specification -> IO (Maybe Specification)
treeBasedMinimalAssumptions context spec =
  case searchTree (guarantees spec) of
    Nothing -> return Nothing
    Just st -> do
      potATree <-
        minimalAssumptionTree context (assumptions spec) (symboltable spec) st
      case potATree of
        Nothing -> return Nothing
        Just aTree ->
          return $ Just $ assumptionTreeToSpec (symboltable spec) [] aTree

-------------------------------------------------------------------------------

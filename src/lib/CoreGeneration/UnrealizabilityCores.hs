-----------------------------------------------------------------------------
-- |
-- Module      : UnrealizabilityCores
-- Description : TSL unrealizable-cores 
-- Maintainer  : Philippe Heim
--
-- Provides methods to generate unrealizability cores
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module CoreGeneration.UnrealizabilityCores
  ( generateCore
  ) where

-----------------------------------------------------------------------------
import CoreGeneration.CoreUtilities
  ( Context(..)
  , logHigh
  , logNormal
  , sortedPowerSet
  )

import Data.Set as Set
  ( Set
  , difference
  , empty
  , filter
  , fromList
  , isSubsetOf
  , map
  , member
  , size
  , toList
  , union
  , unions
  )

import TSL
  ( Formula
  , Formula(..)
  , SignalTerm(..)
  , Specification(..)
  , outputs
  , split
  , toTSL
  , updates
  )

import CoreGeneration.FindFirstConcurrent (incParallelFirst)

-----------------------------------------------------------------------------
-- | 'Query' represents some potential core and is therfore a 'Specification'
type Query = Specification

-----------------------------------------------------------------------------
-- | 'getCores' computes a list of possible queries that can be used to find
-- a unrelaizability core. Note that the queries are sorted in such a way 
-- that the first unrelaizable one is a core with a minimal amount of
-- guarantees.
getCores :: Specification -> [Query]
getCores tsl@Specification {guarantees = g} =
  concatMap
    (\indices ->
       fmap (\spec -> spec {guarantees = addMissingUpdates (guarantees spec)}) $
       (\specs ->
          if length specs == 1
            then specs
            else []) $
       Prelude.filter
         (not . null . guarantees)
         (split (tsl {guarantees = choose indices})))
    (sortedPowerSet $ length g)
  where
    choose indices =
      fmap snd $ Prelude.filter (\(a, _) -> member a indices) $ zip [0 ..] g
    addMissingUpdates choosen =
      let otherUpdates =
            Or $
            TTrue :
            Set.toList
              (Set.difference
                 (getPossibleUpdates (And g) (unions $ fmap outputs choosen))
                 (Set.map (uncurry Update) $ updates (And g)))
       in choosen ++ [otherUpdates]
    --
    getPossibleUpdates :: Ord c => Formula c -> Set c -> Set (Formula c)
    getPossibleUpdates form outps =
      let selfUpdates = Set.map (\o -> (o, Signal o)) (outputs form)
       in Set.map (uncurry Update) $
          Set.filter (\(s, _) -> member s outps) $
          union (updates form) selfUpdates

-----------------------------------------------------------------------------
-- | 'testCoreQuery' checks whether some potential unrealizable core is
-- actually one. To make this more efficent assumption searching is used
-- to find realizbale (non-core) specifications faster. 'testCoreQuery' is
-- provideds a set of assumptions that is surley needed known from the 
-- previous queries.
testCoreQuery ::
     Context -> Set (Formula Int) -> Specification -> IO (Maybe Specification)
testCoreQuery context minimalAssumptions tsl =
  let assmpt = assumptions tsl
      subQueries =
        Prelude.filter
          (\spec -> minimalAssumptions `isSubsetOf` fromList (assumptions spec))
          [ tsl
            { assumptions =
                fmap snd $
                Prelude.filter (\(a, _) -> member a indices) $ zip [0 ..] assmpt
            }
          | indices <-
              Prelude.filter (\set -> size set >= min (length assmpt) 3) $
              sortedPowerSet $ length assmpt
          ]
   in do out <- incParallelFirst (threadPoolSize context) (fmap test subQueries)
         case out of
           Just True -> return Nothing
           _ -> do
             logNormal context $
               "Test Core with " ++
               show (length (assumptions tsl)) ++ " assumptions"
             rel <- tslSpecRealizable context tsl
             if rel
               then return Nothing
               else return (Just tsl)
  where
    test :: Specification -> IO (Maybe Bool)
    test spec = do
      logNormal context $
        "Test Core with " ++ show (length (assumptions spec)) ++ " assumptions"
      rel <- tslSpecRealizable context spec
      if rel
        then return (Just True)
        else return Nothing

-----------------------------------------------------------------------------
-- | 'generateCore' computes for a 'Specification' a minimal 
-- sub-specification with a subset of guarantees that is unrealizable 
-- (if it exists). This is called a unrealizable core. The computation is
-- done using synthesis calls and therfore a 'Context' is needed
generateCore :: Context -> Specification -> IO (Maybe Specification)
generateCore context tsl = do
  let queries = getCores tsl
  genCore' (const empty) queries
  where
    genCore' ::
         (Set (Formula Int) -> Set (Formula Int))
      -> [Query]
      -> IO (Maybe Specification)
    genCore' _ [] = return Nothing
    genCore' minimalAssumptions (q:qr) = do
      logHigh context $ "Check core:\n" ++ toTSL q ++ "\n"
      out <-
        testCoreQuery context (minimalAssumptions (fromList $guarantees q)) q
      case out of
        Nothing ->
          genCore'
            (\set ->
               minimalAssumptions set `union`
               (if fromList (guarantees q) `isSubsetOf` set
                  then fromList $ assumptions q
                  else empty))
            qr
        core -> return core

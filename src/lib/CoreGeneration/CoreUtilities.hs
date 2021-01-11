-------------------------------------------------------------------------------
-- |
-- Module      : CoreUtilities
-- Description : Basic utilities used in different core generation techniques
-- Maintainer  : Philippe Heim
--
-- 'CoreUtilities' provides different functionalities that are used to
-- generate different kind of cores. This includes the wrapping of
-- different IO-operations into a context.
--
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

-------------------------------------------------------------------------------
module CoreGeneration.CoreUtilities
  ( Context(..)
  , Verbosity(..)
  , logNormal
  , logHigh
  , sortedPowerSet
  , optimizeSpec
  ) where

-------------------------------------------------------------------------------
import Data.Set as Set (Set, empty, fromList, insert, notMember, size, toList)

import TSL (Formula(..), Specification(..))

import Control.Monad (when)

-------------------------------------------------------------------------------
-- | 'Context' holds the necessary informations that are used for the
-- synthesis calls and the program logging
data Context =
  Context
    { tslSpecRealizable :: Specification -> IO Bool -- ^ 'tslSpecRealizable'
                                                 -- checks whether some
                                                 -- 'Specification' is
                                                 -- realizable
    , verbosityLevel :: Verbosity -- ^ 'verbosityLevel' defines which output
                                  -- logging verbosity should be applied
    , threadPoolSize :: Int -- ^ 'threadPoolSize' defines how many worker
                            -- threads should be executed at once
    }

-------------------------------------------------------------------------------
-- | 'Verbosity' represents the different verbosity levels for the output
data Verbosity
  -- | Do only output the result
  = SILENT
  -- | Do only output the result and other necessary information
  | QUIET
  -- | Output when important intermediate states are executed
  | STEPWISE
  -- | Output all intermediate steps including the content of the queries
  | DETAILED
  deriving (Eq)

-------------------------------------------------------------------------------
-- | 'logOn' writes a log message if the 'Verbosity' specified in the
-- context is one that has been given as argument.
logOn :: [Verbosity] -> Context -> String -> IO ()
logOn verbosities context logMessage =
  when (verbosityLevel context `elem` verbosities) (putStrLn logMessage)

-------------------------------------------------------------------------------
-- | 'logNormal' writes a log message if the 'Verbosity' is at least
-- 'STEPWISE'.
logNormal :: Context -> String -> IO ()
logNormal = logOn [STEPWISE, DETAILED]

-------------------------------------------------------------------------------
-- | 'logHigh' writes a log message if the 'Verbosity' is 'DETAILED'.
logHigh :: Context -> String -> IO ()
logHigh = logOn [DETAILED]

-------------------------------------------------------------------------------
-- | 'sortedPowerSet' computes the powerset as list of sets such that the
-- list is sorted with ascending size.  Note that this is not done by
-- using 'powerset' and then sorting but in an on-the-fly manner.

sortedPowerSet :: Int -> [Set Int]
sortedPowerSet n = powerSetB n n
  where
    powerSetB :: Int -> Int -> [Set Int]
    powerSetB n bound
      | n < 1 = [empty]
      | n == 1 = empty : [fromList [i] | i <- [0 .. bound - 1]]
      | otherwise =
        let sub = powerSetB (n - 1) bound
            subNew =
              concatMap
                (\s -> [insert i s | i <- [0 .. bound - 1], notMember i s])
                (Prelude.filter (\s -> size s == n - 1) sub)
            new = toList (fromList subNew)
         in sub ++ new

-------------------------------------------------------------------------------
-- | 'optimizeSpec' might do some preprocessing to make a specification more
-- suitable for synthesis.

optimizeSpec :: Specification -> Specification
optimizeSpec Specification {..} =
  Specification
    { symboltable = symboltable
    , assumptions = concatMap splitOnAnd assumptions
    , guarantees = concatMap splitOnAnd guarantees
    }
  where
    splitOnAnd :: Formula Int -> [Formula Int]
    splitOnAnd =
      \case
        And xs -> xs
        f -> [f]

-------------------------------------------------------------------------------

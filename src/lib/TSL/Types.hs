-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Types
-- Maintainer  :  Felix Klein
--
-- Types of the different expressions, semantics and targets.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------

module TSL.Types
  ( ExprType(..)
  , SectionType(..)
  , reducer
  , polyIds
  , prType
  ) where

-----------------------------------------------------------------------------

import Data.Char (chr, ord)

import Data.IntMap (empty, insert, (!))

import qualified Data.IntMap as IM (lookup)

-----------------------------------------------------------------------------

-- | Expression types.

data ExprType =
    TPoly Int
  | TTSL
  | TPattern
  | TSet ExprType
  | TNumber
  | TBoolean
  | TSignal ExprType
  | TFml ExprType ExprType
  deriving (Eq)

-----------------------------------------------------------------------------

instance Show ExprType where
  show = \case
    TSignal s -> "signal " ++ show s
    TFml a b  -> "fn " ++ show a ++ " -> " ++ show b
    TNumber   -> "numerical"
    TBoolean  -> "boolean"
    TTSL      -> "tsl"
    TPattern  -> "pattern"
    TPoly x   -> "a" ++ show x
    TSet x    -> show x ++ " set"

-----------------------------------------------------------------------------

-- | Section types.

data SectionType =
    InitiallyAssume
  | AlwaysAssume Int
  | Assume Int
  | InitiallyGuarantee
  | AlwaysGuarantee Int
  | Guarantee Int
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

-- | Extracts all poly type ids.

polyIds
  :: ExprType -> [Int]

polyIds = reverse . collect []
  where
    collect a = \case
      TSignal x -> collect a x
      TFml x y  -> collect (collect a x) y
      TNumber   -> a
      TBoolean  -> a
      TTSL      -> a
      TPattern  -> a
      TPoly i   -> i:a
      TSet x    -> collect a x

-----------------------------------------------------------------------------

-- | Creates an update mapping that reduces poly identifiers to a
-- uniform range.

reducer
  :: [ExprType] -> Int -> Int

reducer xs =
  let
    f (n,im) i = case IM.lookup i im of
      Nothing -> (n + 1, insert i n im)
      Just _  -> (n, im)
  in
    ((snd $ foldl f (0,empty) $ concatMap polyIds xs) !)

-----------------------------------------------------------------------------

-- | Type printer that encooperates a poly id updater.

prType
  :: (Int -> Int) -> ExprType -> String

prType f = \case
  TNumber   -> "int"
  TTSL      -> "tsl"
  TBoolean  -> "bool"
  TPattern  -> "pattern"
  TSignal s -> prType f s
  TFml a b  -> prType f a ++ " -> " ++ prType f b
  TSet x    -> prType f x ++ " set"
  TPoly i   -> prPoly $ f i

  where
    prPoly i
      | i >= 0 && i < 26 = [chr (i + ord 'a')]
      | otherwise      = "a" ++ show i

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.StringMap
-- Maintainer  :  Felix Klein
--
-- A simple data structure to map strings to integers.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL.StringMap
  ( StringMap
  , empty
  , lookup
  , insert
  , remove
  ) where

-----------------------------------------------------------------------------

import Prelude hiding (lookup)

-----------------------------------------------------------------------------

-- | Internal data structure of the mapping.

data StringMap =
    Empty
  | Leaf (String, Int)
  | Node (Maybe Int, [(Char, StringMap)])
  deriving (Show)

-----------------------------------------------------------------------------

-- | Returns the empty mapping.

empty
  :: StringMap

empty = Empty

-----------------------------------------------------------------------------

-- | Lookups a string in the mapping.

lookup
  :: String -> StringMap -> Maybe Int

lookup str = \case
  Empty       -> Nothing
  Leaf (e,v)
    | e == str   -> Just v
    | otherwise -> Nothing
  Node (v,xs) -> case str of
    []   -> v
    x:xr -> case findMatch x xs of
      Just mapping -> lookup xr mapping
      _            -> Nothing

  where
    findMatch x = \case
      []            -> Nothing
      ((y,n):xr)
        | x == y     -> Just n
        | otherwise -> findMatch x xr

-----------------------------------------------------------------------------

-- | Inserts a new string-int pair to the given mapping. If the mapping
-- already containts the given string, then the corresponding value is
-- updated.

insert
  :: String -> Int -> StringMap -> StringMap

insert s i = \case
  Empty      -> Leaf (s,i)
  Leaf (e,v)
    | e == s     -> Leaf (s,i)
    | otherwise -> case e of
      []     -> Node (Just v, [(head s, Leaf (tail s,i))])
      (x:xr) -> case s of
        []     -> Node (Just i, [(x,Leaf (xr,v))])
        (y:yr)
          | x == y     -> Node (Nothing, [(x, insert yr i (Leaf (xr,v)))])
          | otherwise -> Node (Nothing, [(x, Leaf (xr,v)),(y, Leaf (yr,i))])
  Node (v,xs) -> case s of
    []     -> Node (Just i,xs)
    (x:xr) -> Node (v, add x xr i xs)

  where
    add x xr j = \case
      []         -> [(x, Leaf (xr,j))]
      ((c,n):yr)
        | x == c     -> (c,insert xr j n) : yr
        | otherwise -> (c,n) : add x xr j yr

-----------------------------------------------------------------------------

-- | Removes an entry from the map, if it exists.

remove
  :: String -> StringMap -> StringMap

remove str = \case
  Empty -> Empty
  Leaf (e,v)
    | e == str   -> Empty
    | otherwise -> Leaf (e,v)
  Node (v,xs) -> case str of
    []     -> Node (Nothing, xs)
    (x:xr) -> case del x xr xs of
      [] -> case v of
        Nothing -> Empty
        Just i  -> Leaf ([],i)
      [(c,y)] -> case v of
        Nothing -> case y of
          Leaf (e,w) -> Leaf (c:e,w)
          _          -> Node (Nothing, [(c,y)])
        Just _  -> Node (v, [(c,y)])
      ys  -> Node (v,ys)

  where
    del x xr =
      reverse . foldl (del' x xr) []

    del' x xr a (c,m)
      | x /= c     = (c,m):a
      | otherwise = case remove xr m of
          Empty -> a
          y     -> (c,y):a


-----------------------------------------------------------------------------

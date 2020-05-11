-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Reader
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- The module reads a specification to the internal format.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , TupleSections
  , MultiWayIf
  , LambdaCase

  #-}

-----------------------------------------------------------------------------

module TSL.Reader
  ( fromTSL
  ) where

-----------------------------------------------------------------------------

import TSL.Expression
  ( Expr(..)
  , Expr'(..)
  , subExpressions
  )

import TSL.Error
  ( Error
  )

import TSL.SymbolTable
  ( SymbolTable
  , IdRec(..)
  , Kind(..)
  , symbolTable
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.Reader.Sugar
  ( replaceSugar
  )

import TSL.Parser
  ( parse
  )

import TSL.Logic
  ( Formula(..)
  )

import TSL.Eval
  ( eval
  )

import TSL.Reader.Bindings
  ( specBindings
  )

import TSL.Binding
  ( BoundExpr(..)
  , Binding(..)
  )

import TSL.Reader.InferType
  ( inferTypes
  )

import TSL.Types
  ( ExprType(..)
  , SectionType(..)
  )

import TSL.Reader.Abstraction
  ( abstract
  )

import qualified TSL.Reader.Data as RD
  ( Specification(..)
  )

import Control.Exception
  ( assert
  )

import Data.Function
  ( on
  )

import Data.List
  ( sortBy
  , groupBy
  , partition
  )

import Data.Maybe
  ( fromJust
  , fromMaybe
  )

import Data.Graph
  ( buildG
  , transposeG
  , topSort
  )

import qualified Data.IntMap as IM
  ( IntMap
  , (!)
  , null
  , lookup
  , keys
  , toAscList
  , minViewWithKey
  , maxViewWithKey
  , fromList
  , member
  )

import qualified Data.Array.IArray as A
  ( Array
  , array
  , (!)
  )

import Data.Set
  ( Set
  , empty
  , member
  , insert
  , toList
  )

-----------------------------------------------------------------------------

-- | Parses a TSL specification.

fromTSL
  :: String -> Either Error Specification

fromTSL str =
  -- parse the input
  parse str >>=

  -- replace variable names by a unique identifier
  abstract >>=

  -- replace syntactic sugar constructs for later converison
  replaceSugar >>=

  -- retrieve the bindings of expression variables
  specBindings >>=

  -- infer types and typecheck
  inferTypes >>=

  -- lift reader specification to global specification
  \s@RD.Specification{..} -> do
    let st = symtable s
    es <- eval st $ map snd sections
    return Specification
      { formula     = join $ zip (map fst sections) es
      , assumptions =
          [ initiate (st, f)
          | (st, f) <- zip (map fst sections) es
          , assumption st
          ]
      , guarantees  =
          [ initiate (st, f)
          | (st, f) <- zip (map fst sections) es
          , not (assumption st) ]
      , symboltable = st
      }

  where
    assumption = \case
      InitiallyAssume -> True
      Assume {}       -> True
      AlwaysAssume {} -> True
      _               -> False

--------------------------------------------------------------------------------

join
  :: [(SectionType, Formula Int)] -> Formula Int

join fs = case partition (assumption . fst) fs of
  (_,[])    -> TTrue
  ([],[g])  -> initiate g
  ([],gs)   -> And $ map initiate gs
  ([a],[g]) -> Implies (initiate a) $ initiate g
  ([a],gs)  -> Implies (initiate a) $ And $ map initiate gs
  (as,[g])  -> Implies (And $ map initiate as) $ initiate g
  (as,gs)   -> Implies (And $ map initiate as) $ And $ map initiate gs

  where
    assumption = \case
      InitiallyAssume -> True
      Assume {}       -> True
      AlwaysAssume {} -> True
      _               -> False

-----------------------------------------------------------------------------

initiate
  :: (SectionType, Formula Int) -> Formula Int

initiate (t, fml) = case t of
  InitiallyAssume    -> fml
  Assume n           -> iterate Next fml !! n
  AlwaysAssume n     -> iterate Next (Globally fml) !! n
  InitiallyGuarantee -> fml
  Guarantee n        -> iterate Next fml !! n
  AlwaysGuarantee n  -> iterate Next (Globally fml) !! n

-----------------------------------------------------------------------------

symtable
  :: RD.Specification -> SymbolTable

symtable RD.Specification{..} =
  let
    -- minimal element (taken from 'names' table)
    minkey = key IM.minViewWithKey
    -- maximal element (taken from 'names' table)
    maxkey = key IM.maxViewWithKey
    -- get a list of all top-level expressions in the specification
    es = concatMap (getExprs . bVal) definitions ++ map snd sections
    -- get the list of all outputs of the specification
    so = foldl extractOutputs empty es
    -- get the list of all inputs of the specification
    si = foldl (extractInputs scopes types) empty es
    -- maps every output to its dependendencies, i.e., the functions
    -- and inputs it can be computed from
    oa =
      IM.fromList
      $ map (\x -> (fst $ head x, map snd x))
      $ groupBy ((==) `on` fst)
      $ sortBy (compare `on` fst)
      $ toList
      $ foldl extractOutputAssignments empty es

    idT i = {-updType si so i $ -} fromMaybe (TPoly i) $ IM.lookup i types

    -- list of identifiers sorted by dependencies
    is' =
      topSort
      $ transposeG
      $ buildG (minkey, maxkey)
      $ concatMap (\(i,xs) -> map (i,) xs)
      $ IM.toAscList dependencies

    -- update mapping accoording to dependency order
    uD = (IM.fromList $ zip is' [minkey, minkey + 1 .. maxkey])

    -- update array according to depencency order
    aD :: A.Array Int Int
    aD = A.array (minkey, maxkey) $ IM.toAscList uD

    -- depencency ordering
    cmpD :: Int -> Int -> Ordering
    cmpD x y = compare (aD A.! x) (aD A.! y)

    -- ordering accoring to expression class, using dependency
    -- ordering for equivalent classes
    cmp a b =
      let
        aT = tEn so si a $ idT a
        bT = tEn so si b $ idT b
      in if
        | aT /= bT   -> compare aT bT
        | otherwise -> cmpD a b

    -- sorted identifiers by above ordering
    is = sortBy cmp $ IM.keys names
  in
    symbolTable
      $ A.array (minkey, maxkey)
      $ map (\i -> (i, entry oa si so i)) is

  where
    getExprs = \case
      GuardedBinding xs    -> xs
      PatternBinding x y   -> [x,y]
      SetBinding x         -> [x]
      RangeBinding x _ y _ -> [x,y]

    entry oa si so i =
      let
        t = fromMaybe (TPoly i) $ IM.lookup i types
        as = fromMaybe [] $ IM.lookup i arguments
        ds = fromMaybe [] $ IM.lookup i dependencies
      in
        IdRec
          { idName     = assert (IM.member i names) (names IM.! i)
          , idPos      = assert (IM.member i positions) $
                           Just (positions IM.! i)
          , idArgs     = as
          , idBindings = assert (IM.member i bindings) $
                           Just (bindings IM.! i)
          , idType     = t
          , idDeps     = if
              | member i so -> case IM.lookup i oa of
                  Just xs -> i : xs
                  Nothing -> ds
              | otherwise   -> ds
          , idKind     = case IM.lookup i scopes of
              Just ()          -> Internal
              Nothing
                | member i so -> Output
                | member i si -> Input
                | predicate t -> Predicate
                | otherwise   -> case t of
                    TSignal {} -> Constant
                    _          -> Function
          }

    predicate = \case
      TSignal TBoolean -> True
      TFml _ x         -> predicate x
      _                -> False

    tEn
      :: Set Int -> Set Int -> Int -> ExprType -> Int

    tEn so si i = \case
      TSignal {}
        | member i si           -> 0
        | member i so           -> 1
        | otherwise             -> 2
      TFml _ (TSignal TBoolean) -> 3
      TFml {}                   -> 4
      _                         -> 5

    key f
      | IM.null names = 0
      | otherwise     =
          fst $ fst $ fromJust $ f names

-----------------------------------------------------------------------------

extractInputs
  :: IM.IntMap () -> IM.IntMap ExprType -> Set Int -> Expr Int -> Set Int

extractInputs sc tt a e@Expr{..} = case expr of
  BaseId i -> case assert (IM.member i tt) (tt IM.! i) of
    TSignal {} -> case IM.lookup i sc of
      Nothing -> insert i a
      Just ()  -> a
    _          -> a
  _             -> foldl (extractInputs sc tt) a $ subExpressions e

-----------------------------------------------------------------------------

extractOutputs
  :: Set Int -> Expr Int -> Set Int

extractOutputs a e@Expr{..} = case expr of
  BaseUpd _ x   -> insert x a
  _             -> foldl extractOutputs a $ subExpressions e

-----------------------------------------------------------------------------

extractOutputAssignments
  :: Set (Int, Int) -> Expr Int -> Set (Int, Int)

extractOutputAssignments a e@Expr{..} = case expr of
  BaseUpd x i -> collect i a x
  _           -> foldl extractOutputAssignments a $ subExpressions e

  where
    collect i a e@Expr{..} = case expr of
      BaseFn x y  -> collect i (collect i a x) y
      BaseConFn j -> insert (i,j) a
      BaseId j    -> insert (i,j) a
      _           -> foldl (collect i) a $ subExpressions e

-----------------------------------------------------------------------------

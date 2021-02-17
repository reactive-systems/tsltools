-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Reader.Abstraction
-- Maintainer  :  Felix Klein
--
-- Abstracts from identifier names to integer IDs.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-----------------------------------------------------------------------------

module TSL.Reader.Abstraction
  ( abstract
  ) where

-----------------------------------------------------------------------------

import TSL.Binding (Binding(..), BoundExpr(..))

import TSL.Expression (Expr(..), Expr'(..), ExprPos(..))

import TSL.Reader.Data
  ( ArgumentTable
  , NameTable
  , PositionTable
  , Specification(..)
  )

import TSL.Error (Error, errConflict, errPattern, errUnknown)

import qualified TSL.StringMap as SM (StringMap, empty, insert, lookup, remove)

import qualified TSL.Parser.Data as PD (Specification(..))

import Data.Maybe (mapMaybe)

import Control.Monad.State (StateT(..), evalStateT, foldM, get, put)

import Control.Exception (assert)

import qualified Data.IntMap.Strict as IM (empty, insert, lookup)

-----------------------------------------------------------------------------

type Abstractor a b = a -> StateT ST (Either Error) b

-----------------------------------------------------------------------------

data ST = ST
  { count  :: Int
  , tIndex :: SM.StringMap
  , tName  :: NameTable
  , tPos   :: PositionTable
  , tArgs  :: ArgumentTable
  }

-----------------------------------------------------------------------------

-- | Abstracts from identifiers represeted by strings to identifiers
-- represented by integers. Additionally, a mapping from the integer
-- representation back to the string representation as well as mapping
-- to possible arguments and the position in the source file is
-- created.

abstract
  :: PD.Specification -> Either Error Specification

abstract spec =
  evalStateT (abstractSpec spec)
    ST { count  = 1
       , tIndex = SM.empty
       , tName  = IM.empty
       , tPos   = IM.empty
       , tArgs  = IM.empty
       }

-----------------------------------------------------------------------------

abstractSpec
  :: Abstractor PD.Specification Specification

abstractSpec PD.Specification{..} = do
  -- add the names of all bindings first, so they are known as soon as
  -- we evaluate the bound expressions
  mapM_ (\Binding{..} -> add (bIdent,bPos)) definitions
  -- then evaluate the bound expressions
  definitions' <- mapM abstractBinding definitions
  -- finally evaluate the formulas
  sections' <- mapM (\(t,e) -> (t,) <$> abstractExpr e) sections
  -- return the new specification
  ST{..} <- get
  return Specification
    { definitions  = definitions'
    , sections     = sections'
    , bindings     = IM.empty
    , names        = tName
    , positions    = tPos
    , arguments    = tArgs
    , dependencies = IM.empty
    , types        = IM.empty
    , scopes       = IM.empty
    , exprRange    = (0,0)
    }

-----------------------------------------------------------------------------

-- | Shortcut to lookup a string identifier in the global lookup table.

lookupName
  :: Abstractor String (Maybe Int)

lookupName str =
  get >>= \ST{..} -> return $ SM.lookup str tIndex

-----------------------------------------------------------------------------

-- | Adds a new idententifier name to the lookup tables.

add
  :: Abstractor (String, ExprPos) Int

add (str,pos) =
  lookupName str >>= \case
    Nothing ->
      get >>= \ST{..} -> put ST
        { count  = count + 1
        , tIndex = SM.insert str count tIndex
        , tName  = IM.insert count str tName
        , tPos   = IM.insert count pos tPos
        , tArgs  = tArgs
        }
      >> return count
    Just j ->
      get >>= \ST{..} -> case IM.lookup j tPos of
        Just str' -> errConflict str str' pos
        Nothing   -> assert False undefined

-----------------------------------------------------------------------------

-- | Removes an idententifier from the lookup tables.

del
  :: Abstractor String ()

del str =
  lookupName str >>= \case
    Just _  ->
      get >>= \st@ST{..} -> put st
        { tIndex = SM.remove str tIndex
        }
    Nothing -> return ()

-----------------------------------------------------------------------------

-- | Exchanges the string identifiers of an expression binding by the
-- respective int identifiers.

abstractBinding
  :: Abstractor (Binding String) (Binding Int)

abstractBinding Binding{..} = do
  -- add the arguments
  as <- mapM add bArgs
  -- evaluate the bound expression
  be <- case bVal of
    GuardedBinding xs        -> GuardedBinding <$> mapM abstractExpr xs
    PatternBinding x y       -> do
      x' <- abstractExpr x
      y' <- abstractExpr y
      return $ PatternBinding x' y'
    SetBinding x             -> SetBinding <$> abstractExpr x
    RangeBinding e1 f1 e2 f2 -> do
      e1' <- abstractExpr e1
      e2' <- abstractExpr e2
      return $ RangeBinding e1' f1 e2' f2

  -- remove the arguments again
  mapM_ (del . fst) bArgs

  -- get the identifier of the binding
  j <- lookupName bIdent >>= \case
    Just x  -> return x
    Nothing -> errUnknown bIdent bPos

  -- add the arguments to the binding
  get >>= \st@ST{..} -> put st
    { tArgs = IM.insert j as tArgs }

  return Binding
    { bIdent = j
    , bArgs  = zip as $ map snd bArgs
    , bPos   = bPos
    , bVal   = be
    }

-----------------------------------------------------------------------------

-- | Updates the identifiers within an expression.

abstractExpr
  :: Abstractor (Expr String) (Expr Int)

abstractExpr Expr{..} = case expr of
  BaseWild             -> return $ expr' BaseWild
  BaseTrue             -> return $ expr' BaseTrue
  BaseFalse            -> return $ expr' BaseFalse
  BaseOtherwise        -> return $ expr' BaseOtherwise
  BaseCon x            -> return $ expr' (BaseCon x)
  BlnNot x             -> lift' BlnNot x
  NumSMin x            -> lift' NumSMin x
  NumSMax x            -> lift' NumSMax x
  NumSSize x           -> lift' NumSSize x
  TslNext x            -> lift' TslNext x
  TslPrevious x        -> lift' TslPrevious x
  TslGlobally x        -> lift' TslGlobally x
  TslFinally x         -> lift' TslFinally x
  TslHistorically x    -> lift' TslHistorically x
  TslOnce x            -> lift' TslOnce x
  BaseFn x y           -> lift2' BaseFn x y
  NumPlus x y          -> lift2' NumPlus x y
  NumMinus x y         -> lift2' NumMinus x y
  NumMul x y           -> lift2' NumMul x y
  NumDiv x y           -> lift2' NumDiv x y
  NumMod x y           -> lift2' NumMod x y
  SetCup x y           -> lift2' SetCup x y
  SetCap x y           -> lift2' SetCap x y
  SetMinus x y         -> lift2' SetMinus x y
  BlnEQ x y            -> lift2' BlnEQ x y
  BlnNEQ x y           -> lift2' BlnNEQ x y
  BlnGE x y            -> lift2' BlnGE x y
  BlnGEQ x y           -> lift2' BlnGEQ x y
  BlnLE x y            -> lift2' BlnLE x y
  BlnLEQ x y           -> lift2' BlnLEQ x y
  BlnElem x y          -> lift2' BlnElem x y
  BlnOr x y            -> lift2' BlnOr x y
  BlnAnd x y           -> lift2' BlnAnd x y
  BlnImpl x y          -> lift2' BlnImpl x y
  BlnEquiv x y         -> lift2' BlnEquiv x y
  TslRNext x y         -> lift2' TslRNext x y
  TslRPrevious x y     -> lift2' TslRPrevious x y
  TslRGlobally x y     -> lift2' TslRGlobally x y
  TslRFinally x y      -> lift2' TslRFinally x y
  TslRHistorically x y -> lift2' TslRHistorically x y
  TslROnce x y         -> lift2' TslROnce x y
  TslUntil x y         -> lift2' TslUntil x y
  TslWeak x y          -> lift2' TslWeak x y
  TslAsSoonAs x y      -> lift2' TslAsSoonAs x y
  TslRelease x y       -> lift2' TslRelease x y
  TslSince x y         -> lift2' TslSince x y
  TslTriggered x y     -> lift2' TslTriggered x y
  Pattern x y          -> lift2' Pattern x y
  NumRPlus xs x        -> cond NumRPlus xs x
  NumRMul xs x         -> cond NumRMul xs x
  SetRCup xs x         -> cond SetRCup xs x
  SetRCap xs x         -> cond SetRCap xs x
  BlnROr xs x          -> cond BlnROr xs x
  BlnRAnd xs x         -> cond BlnRAnd xs x
  SetExplicit xs       -> do
    xs' <- mapM abstractExpr xs
    return $ expr' $ SetExplicit xs'
  SetRange x y z       -> do
    x' <- abstractExpr x
    y' <- abstractExpr y
    z' <- abstractExpr z
    return $ expr' $ SetRange x' y' z'
  Colon (Expr v i p) z -> case v of
    Pattern x y -> do
      x' <- abstractExpr x
      zs <- getPatternIds [] y
      mapM_ add zs
      y' <- abstractExpr y
      z' <- abstractExpr z
      mapM_ (del . fst) zs
      return $ expr' $ Colon (Expr (Pattern x' y') i p) z'
    _ -> lift2' Colon (Expr v i p) z
  BaseUpd y x          -> do
    y' <- abstractExpr y
    x' <- lookupName x >>= \case
      Nothing -> add (x,srcPos)
      Just i  -> return i
    return $ expr' $ BaseUpd y' x'
  BaseId x             -> do
    x' <- lookupName x >>= \case
      Nothing -> add (x, srcPos)
      Just i  -> return i
    return $ expr' $ BaseId x'
  BaseConFn x          -> do
    x' <- lookupName x >>= \case
      Nothing -> add (x, srcPos)
      Just i  -> return i
    return $ expr' $ BaseConFn x'

  where
    lift' c x = do
      x' <- abstractExpr x
      return $ Expr (c x') exprId srcPos

    lift2' c x y = do
      x' <- abstractExpr x
      y' <- abstractExpr y
      return $ Expr (c x' y') exprId srcPos

    cond c xs x = do
      let is = mapMaybe getId xs
      mapM_ add is
      xs' <- mapM abstractExpr xs
      x' <- abstractExpr x
      mapM_ (del . fst) is
      return $ Expr (c xs' x') exprId srcPos

    getId Expr{..} = case expr of
      BlnElem y _ -> isid y
      BlnLE n _   -> range n
      BlnLEQ n _  -> range n
      _           -> Nothing

    range Expr{..} = case expr of
      BlnLE _ m  -> isid m
      BlnLEQ _ m -> isid m
      _          -> Nothing

    isid Expr{..} = case expr of
      BaseId s -> Just (s,srcPos)
      _        -> Nothing

    getPatternIds a Expr{..} = case expr of
      BaseWild             -> return a
      BaseTrue             -> return a
      BaseFalse            -> return a
      BaseOtherwise        -> return a
      BaseId str           -> return $ (str,srcPos) : a
      BlnNot x             -> getPatternIds a x
      BlnOr x y            -> foldM getPatternIds a [x,y]
      BlnAnd x y           -> foldM getPatternIds a [x,y]
      BlnImpl x y          -> foldM getPatternIds a [x,y]
      BlnEquiv x y         -> foldM getPatternIds a [x,y]
      TslNext x            -> getPatternIds a x
      TslRNext _ x         -> getPatternIds a x
      TslPrevious x        -> getPatternIds a x
      TslRPrevious _ x     -> getPatternIds a x
      TslGlobally x        -> getPatternIds a x
      TslRGlobally _ x     -> getPatternIds a x
      TslFinally x         -> getPatternIds a x
      TslRFinally _ x      -> getPatternIds a x
      TslHistorically x    -> getPatternIds a x
      TslRHistorically _ x -> getPatternIds a x
      TslOnce x            -> getPatternIds a x
      TslROnce _ x         -> getPatternIds a x
      TslUntil x y         -> foldM getPatternIds a [x,y]
      TslWeak x y          -> foldM getPatternIds a [x,y]
      TslAsSoonAs x y      -> foldM getPatternIds a [x,y]
      TslRelease x y       -> foldM getPatternIds a [x,y]
      TslSince x y         -> foldM getPatternIds a [x,y]
      TslTriggered x y     -> foldM getPatternIds a [x,y]
      _                    -> errPattern srcPos

    expr' x = Expr x exprId srcPos

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Reader.Bindings
-- Maintainer  :  Felix Klein
--
-- Extracts the static expression bindings from the specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , LambdaCase
  , TupleSections

  #-}

-----------------------------------------------------------------------------

module TSL.Reader.Bindings
  ( specBindings
  ) where

-----------------------------------------------------------------------------

import TSL.Binding
  ( Binding(..)
  , BoundExpr(..)
  )

import TSL.Expression
  ( ExprId
  , Expr(..)
  , Expr'(..)
  , subExpressions
  )

import TSL.Reader.Data
  ( Specification(..)
  , ExpressionTable
  , NameTable
  , PositionTable
  , ArgumentTable
  , ScopeTable
  )

import TSL.Error
  ( Error
  , errConditional
  , errCircularDep
  )

import Data.Graph
  ( buildG
  , scc
  )

import Data.Tree
  ( flatten
  )

import Control.Monad.State
  ( StateT(..)
  , execStateT
  , put
  , get
  , when
  )

import Control.Exception
  ( assert
  )

import Data.IntMap.Strict
  ( (!)
  , member
  , empty
  , insert
  , toList
  , findMax
  )

import qualified Data.IntMap.Strict as IM
  ( map
  , lookup
  )

import Data.Set
  ( elems
  , fromList
  )

import TSL.Types
  ( SectionType
  )

-----------------------------------------------------------------------------

type GetBindings a = StateT ST (Either Error) a

-----------------------------------------------------------------------------

data ST = ST
  { tBinding :: ExpressionTable
  , tName :: NameTable
  , tPos :: PositionTable
  , tArgs :: ArgumentTable
  , tInternal :: ScopeTable
  }

-----------------------------------------------------------------------------

-- | Extracts the static expression bindings from the specification and
-- stores them in a corresponding mapping. Furthermore, depencency
-- constraints are already checked.

specBindings
  :: Specification -> Either Error Specification

specBindings spec' = do
  spec@Specification{..} <- fixExprIds spec'

  ST{..} <-
    execStateT (specificationBindings spec) ST
      { tBinding  = empty
      , tName     = names
      , tPos      = positions
      , tArgs     = arguments
      , tInternal = empty
      }

  checkCircularDeps spec
    { bindings     = tBinding
    , scopes       = tInternal
    , dependencies = IM.map deps tBinding
    }

-----------------------------------------------------------------------------

specificationBindings
  :: Specification -> GetBindings ()

specificationBindings Specification{..} = do
  mapM_ binding definitions
  mapM_ (exprBindings . snd) sections

-----------------------------------------------------------------------------

binding
  :: Binding Int -> GetBindings ()

binding Binding{..} = do
  st@ST{..} <- get

  case IM.lookup bIdent tBinding of
    Nothing -> put st
      { tBinding = insert bIdent bVal tBinding
      , tInternal =
          foldl
            (\a i -> insert i () a)
            (insert bIdent () tInternal)
            (map fst bArgs)
      }
    Just _  -> return ()

  case bVal of
    GuardedBinding xs    -> mapM_ exprBindings xs
    PatternBinding x y   -> exprBindings x >> exprBindings y
    SetBinding x         -> exprBindings x
    RangeBinding x _ y _ -> exprBindings x >> exprBindings y

-----------------------------------------------------------------------------

exprBindings
  :: Expr Int -> GetBindings ()

exprBindings e@Expr{..} = case expr of
  NumRPlus xs x        -> mapM_ conditional xs >> exprBindings x
  NumRMul xs x         -> mapM_ conditional xs >> exprBindings x
  SetRCup xs x         -> mapM_ conditional xs >> exprBindings x
  SetRCap xs x         -> mapM_ conditional xs >> exprBindings x
  BlnRAnd xs x         -> mapM_ conditional xs >> exprBindings x
  BlnROr xs x          -> mapM_ conditional xs >> exprBindings x
  TslRNext _ x         -> exprBindings x
  TslRPrevious _ x     -> exprBindings x
  TslRGlobally _ x     -> exprBindings x
  TslRFinally _ x      -> exprBindings x
  TslRHistorically _ x -> exprBindings x
  TslROnce _ x         -> exprBindings x
  Colon w@(Expr v _ _) z -> case v of
    Pattern x y -> addPatternIds (PatternBinding x y) y >> exprBindings z
    _ -> exprBindings w >> exprBindings z
  _                    -> mapM_ exprBindings $ subExpressions e

  where
    addPatternIds
      :: BoundExpr Int -> Expr Int -> GetBindings ()

    addPatternIds b Expr{..} = case expr of
      BaseWild             -> return ()
      BaseTrue             -> return ()
      BaseFalse            -> return ()
      BaseOtherwise        -> return ()
      BaseId i             ->
        get >>= \st@ST{..} -> put st
          { tBinding = insert i b tBinding
          , tInternal = insert i () tInternal
          }
      BlnNot x             -> addPatternIds b x
      BlnOr x y            -> addPatternIds b x >> addPatternIds b y
      BlnAnd x y           -> addPatternIds b x >> addPatternIds b y
      BlnImpl x y          -> addPatternIds b x >> addPatternIds b y
      BlnEquiv x y         -> addPatternIds b x >> addPatternIds b y
      TslNext x            -> addPatternIds b x
      TslRNext _ x         -> addPatternIds b x
      TslPrevious x        -> addPatternIds b x
      TslRPrevious _ x     -> addPatternIds b x
      TslGlobally x        -> addPatternIds b x
      TslRGlobally _ x     -> addPatternIds b x
      TslFinally x         -> addPatternIds b x
      TslRFinally _ x      -> addPatternIds b x
      TslHistorically x    -> addPatternIds b x
      TslRHistorically _ x -> addPatternIds b x
      TslOnce x            -> addPatternIds b x
      TslROnce _ x         -> addPatternIds b x
      TslUntil x y         -> addPatternIds b x >> addPatternIds b y
      TslWeak x y          -> addPatternIds b x >> addPatternIds b y
      TslAsSoonAs x y      -> addPatternIds b x >> addPatternIds b y
      TslRelease x y       -> addPatternIds b x >> addPatternIds b y
      TslSince x y         -> addPatternIds b x >> addPatternIds b y
      TslTriggered x y     -> addPatternIds b x >> addPatternIds b y
      _                    -> assert False undefined

    conditional (Expr x _ _) = case x of
      BlnElem (Expr l _ _) s -> case l of
        BaseId i ->
          get >>= \st@ST{..} -> put st
            { tBinding =
                insert i (SetBinding s) tBinding
            , tInternal =
                insert i () tInternal
            }
        _ -> errConditional srcPos
      BlnLE (Expr s _ _) r   -> case s of
        BlnLE l (Expr j _ _)  -> case j of
          BaseId i ->
            get >>= \st@ST{..} -> put st
              { tBinding =
                  insert i (RangeBinding l (+1) r (+(-1))) tBinding
              , tInternal =
                  insert i () tInternal
              }
          _ -> errConditional srcPos
        BlnLEQ l (Expr j _ _) -> case j of
          BaseId i ->
            get >>= \st@ST{..} -> put st
              { tBinding =
                  insert i (RangeBinding l id r (+(-1))) tBinding
              , tInternal =
                  insert i () tInternal
              }
          _ -> errConditional srcPos
        _ -> errConditional srcPos
      BlnLEQ (Expr s _ _) r   -> case s of
        BlnLE l (Expr j _ _)  -> case j of
          BaseId i ->
            get >>= \st@ST{..} -> put st
              { tBinding =
                  insert i (RangeBinding l (+1) r id) tBinding
              , tInternal =
                  insert i () tInternal
              }
          _ -> errConditional srcPos
        BlnLEQ l (Expr j _ _) -> case j of
          BaseId i ->
            get >>= \st@ST{..} -> put st
              { tBinding =
                  insert i (RangeBinding l id r id) tBinding
              , tInternal =
                  insert i () tInternal
              }
          _ -> errConditional srcPos
        _ -> errConditional srcPos
      _ -> errConditional srcPos

-----------------------------------------------------------------------------

deps
  :: BoundExpr Int -> [Int]

deps = elems . fromList . deps' []
  where
    deps' a = \case
      GuardedBinding xs    -> foldl deps'' a xs
      PatternBinding x y   -> deps'' (deps'' a x) y
      SetBinding x         -> deps'' a x
      RangeBinding x _ y _ -> deps'' (deps'' a x) y

    deps'' a e = case expr e of
      NumRPlus xs x -> foldl conditional (deps'' a x) xs
      NumRMul xs x  -> foldl conditional (deps'' a x) xs
      SetRCup xs x  -> foldl conditional (deps'' a x) xs
      SetRCap xs x  -> foldl conditional (deps'' a x) xs
      BlnRAnd xs x  -> foldl conditional (deps'' a x) xs
      BlnROr xs x   -> foldl conditional (deps'' a x) xs
      BaseUpd x y   -> deps'' (y:a) x
      BaseId x      -> x:a
      BaseConFn x   -> x:a
      Colon x y     -> case expr x of
        Pattern z _ -> deps'' (deps'' a z) y
        _           -> deps'' (deps'' a x) y
      _             -> foldl deps'' a $ subExpressions e

    conditional a x = case expr x of
      BlnElem _ y -> deps'' a y
      BlnLE s r   -> case expr s of
        BlnLE l _  -> deps'' (deps'' a r) l
        BlnLEQ l _ -> deps'' (deps'' a r) l
        _          -> deps'' a x
      BlnLEQ s r  -> case expr s of
        BlnLE l _  -> deps'' (deps'' a r) l
        BlnLEQ l _ -> deps'' (deps'' a r) l
        _          -> deps'' a x
      _           -> deps'' a x

-----------------------------------------------------------------------------

checkCircularDeps
  :: Specification -> Either Error Specification

checkCircularDeps s@Specification{..} = do
  let
    ys = concatMap (\(i,zs) -> map (i,) zs) $
         filter isunary $ toList dependencies

    minkey
      | null ys   = 0
      | otherwise = minimum $ map (uncurry min) ys

    maxkey
      | null ys   = 0
      | otherwise = maximum $ map (uncurry max) ys

    c = map flatten $ scc $ buildG (minkey,maxkey) ys

  mapM_ check c
  mapM_ checkSingelton ys
  return s

  where
    isunary (x,_) = case IM.lookup x arguments of
      Just xs -> null xs
      Nothing -> assert False undefined

    check = \case
      []     -> return ()
      [_]    -> return ()
      (x:xr) ->
        let
          p = assert (member x positions) (positions ! x)
          ys = map (\i -> ( assert (member i names) (names ! i)
                         , assert (member i positions) (positions ! i)
                         )) (x:xr)
        in
          errCircularDep ys p

    checkSingelton (i,j) =
      when (i == j) $
        errCircularDep
          [( assert (member i names) (names ! i)
           , assert (member i positions) (positions ! i)
           )]
          (assert (member i positions) (positions ! i))

-----------------------------------------------------------------------------

-- | Assign every expression of the input specification a unique id.

fixExprIds
  :: Specification -> Either Error Specification

fixExprIds s@Specification{..} = do
  let j = (+1) $ fst $ findMax names
  (ds,n) <- runStateT (mapM fixBindingIds definitions) j
  (xs,m) <- runStateT (mapM fixSectionIds sections) n
  return s
    { definitions = ds
    , sections = xs
    , exprRange = (m - j, m - 1)
    }

  where
    fixBindingIds
      :: Binding Int -> StateT ExprId (Either Error) (Binding Int)

    fixBindingIds b@Binding{..} = case bVal of
      GuardedBinding xs    -> do
        es <- mapM updExpr xs
        return b { bVal = GuardedBinding es }
      PatternBinding x y -> do
        x' <- updExpr x
        y' <- updExpr y
        return b { bVal = PatternBinding x' y' }
      SetBinding x         -> do
        x' <- updExpr x
        return b { bVal = SetBinding x' }
      RangeBinding x f y g -> do
        x' <- updExpr x
        y' <- updExpr y
        return b { bVal = RangeBinding x' f y' g }

    fixSectionIds
      :: (SectionType, Expr Int)
      -> StateT ExprId (Either Error) (SectionType, Expr Int)

    fixSectionIds (t,e) = do
      e' <- updExpr e
      return (t,e')


    updExpr
      :: Expr Int -> StateT ExprId (Either Error) (Expr Int)

    updExpr e = do
      n <- get
      put (n + 1)
      e' <- case expr e of
        BaseWild             -> return BaseWild
        BaseTrue             -> return BaseTrue
        BaseFalse            -> return BaseFalse
        BaseCon i            -> return $ BaseCon i
        BaseOtherwise        -> return BaseOtherwise
        BaseId i             -> return $ BaseId i
        BaseConFn i          -> return $ BaseConFn i
        BaseUpd x i          -> updExpr1 (`BaseUpd` i) x
        NumSMin x            -> updExpr1 NumSMin x
        NumSMax x            -> updExpr1 NumSMax x
        NumSSize x           -> updExpr1 NumSSize x
        BlnNot x             -> updExpr1 BlnNot x
        TslNext x            -> updExpr1 TslNext x
        TslPrevious x        -> updExpr1 TslPrevious x
        TslGlobally x        -> updExpr1 TslGlobally x
        TslFinally x         -> updExpr1 TslFinally x
        TslHistorically x    -> updExpr1 TslHistorically x
        TslOnce x            -> updExpr1 TslOnce x
        NumPlus x y          -> updExpr2 NumPlus x y
        NumMinus x y         -> updExpr2 NumMinus x y
        NumMul x y           -> updExpr2 NumMul x y
        NumDiv x y           -> updExpr2 NumDiv x y
        NumMod x y           -> updExpr2 NumMod x y
        SetCup x y           -> updExpr2 SetCup x y
        SetCap x y           -> updExpr2 SetCap x y
        SetMinus x y         -> updExpr2 SetMinus x y
        BlnEQ x y            -> updExpr2 BlnEQ x y
        BlnNEQ x y           -> updExpr2 BlnNEQ x y
        BlnGE x y            -> updExpr2 BlnGE x y
        BlnGEQ x y           -> updExpr2 BlnGEQ x y
        BlnLE x y            -> updExpr2 BlnLE x y
        BlnLEQ x y           -> updExpr2 BlnLEQ x y
        BlnElem x y          -> updExpr2 BlnElem x y
        BlnOr x y            -> updExpr2 BlnOr x y
        BlnAnd x y           -> updExpr2 BlnAnd x y
        BlnImpl x y          -> updExpr2 BlnImpl x y
        BlnEquiv x y         -> updExpr2 BlnEquiv x y
        TslRNext x y         -> updExpr2 TslRNext x y
        TslRPrevious x y     -> updExpr2 TslRPrevious x y
        TslRGlobally x y     -> updExpr2 TslRGlobally x y
        TslRFinally x y      -> updExpr2 TslRFinally x y
        TslRHistorically x y -> updExpr2 TslRHistorically x y
        TslROnce x y         -> updExpr2 TslROnce x y
        TslUntil x y         -> updExpr2 TslUntil x y
        TslWeak x y          -> updExpr2 TslWeak x y
        TslAsSoonAs x y      -> updExpr2 TslAsSoonAs x y
        TslRelease x y       -> updExpr2 TslRelease x y
        TslSince x y         -> updExpr2 TslSince x y
        TslTriggered x y     -> updExpr2 TslTriggered x y
        Colon x y            -> updExpr2 Colon x y
        Pattern x y          -> updExpr2 Pattern x y
        SetRange x y z       -> updExpr3 SetRange x y z
        SetExplicit xs       -> updExprN SetExplicit xs
        BaseFn x y           -> updExpr2 BaseFn x y
        NumRPlus xs x        -> updExprM NumRPlus xs x
        NumRMul xs x         -> updExprM NumRMul xs x
        SetRCup xs x         -> updExprM SetRCup xs x
        SetRCap xs x         -> updExprM SetRCap xs x
        BlnROr xs x          -> updExprM BlnROr xs x
        BlnRAnd xs x         -> updExprM BlnRAnd xs x

      return e
        { expr = e'
        , exprId = n
        }

    updExpr1 c x =
      c <$> updExpr x

    updExpr2 c x y = do
      x' <- updExpr x
      y' <- updExpr y
      return $ c x' y'

    updExpr3 c x y z = do
      x' <- updExpr x
      y' <- updExpr y
      z' <- updExpr z
      return $ c x' y' z'

    updExprN c xs =
      c <$> mapM updExpr xs

    updExprM c xs x = do
      xs' <- mapM updExpr xs
      x' <- updExpr x
      return $ c xs' x'

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.Reader.InferType
-- Maintainer  :  Felix Klein
--
-- Infers and checks types of all bound expressions.
module TSL.Reader.InferType
  ( inferTypes,
  )
where

-----------------------------------------------------------------------------

import Control.Exception (assert)
import Control.Monad (foldM_, void)
import Control.Monad.State (StateT (..), execStateT, get, gets, modify, put)
import Data.Graph (Graph, buildG, topSort, transposeG)
import Data.IntMap.Strict (IntMap, empty, fromList, insert, keys, member, (!))
import qualified Data.IntMap.Strict as IM (lookup)
import qualified Data.Set as S (fromList, intersection, toList)
import TSL.Binding (Binding (..), BoundExpr (..))
import TSL.Error (Error, errExpect, errRange)
import TSL.Expression (Expr (..), Expr' (..), Expression)
import TSL.Reader.Data (ArgumentTable, Specification (..), TypeTable)
import TSL.Types (ExprType (..))

-----------------------------------------------------------------------------

type Id = Int

-----------------------------------------------------------------------------

data ST = ST
  { -- | number of used type identifiers
    tCount :: Int,
    -- | the type table to be generated
    tTypes :: TypeTable,
    -- | the argument table for function identifiers
    targs :: ArgumentTable,
    -- | the overall specification record
    spec :: Specification,
    -- | recusion indicator
    curBinding :: Id
  }

-----------------------------------------------------------------------------

type TypeCheck a = StateT ST (Either Error) a

-----------------------------------------------------------------------------

-- | Infers and checks types of all bound expressions as well as of
-- the assumptions, the invariants and the guarantees. Additionally, a
-- mapping from each identifier to its type is created.
inferTypes ::
  Specification -> Either Error Specification
inferTypes s@Specification {..} = do
  tt <-
    execStateT
      inferTypeSpec
      ST
        { tCount = (+ 1) $ snd exprRange,
          tTypes = empty,
          targs = arguments,
          spec = s,
          curBinding = -1
        }

  return
    s
      { types = finalize $ tTypes tt
      }

-----------------------------------------------------------------------------

finalize ::
  TypeTable -> TypeTable
finalize tt =
  foldl
    (\a i -> insert i (resolveT a (assert (member i a) (a ! i))) a)
    tt
    (keys tt)
  where
    resolveT ta = \case
      TSet t -> TSet $ resolveT ta t
      TSignal t -> liftS $ resolveT ta t
      TFml t t' ->
        let tf = TFml (resolveT ta t) $ resolveT ta t'
         in if
                | onSignalLevel tf -> liftS tf
                | otherwise -> tf
      TPoly i -> case IM.lookup i ta of
        Nothing -> TPoly i
        Just (TSet t) -> TSet $ resolveT ta t
        Just (TSignal t) -> liftS $ resolveT ta t
        Just (TFml t t') ->
          let tf = TFml (resolveT ta t) $ resolveT ta t'
           in if
                  | onSignalLevel tf -> liftS tf
                  | otherwise -> tf
        Just (TPoly j)
          | i == j -> TPoly i
          | otherwise -> assert (j < i) $ resolveT ta $ TPoly j
        Just t -> t
      t -> t

-----------------------------------------------------------------------------

inferTypeSpec ::
  TypeCheck ()
inferTypeSpec = do
  -- get the specification
  s@Specification {..} <- gets spec
  -- get the global bindings
  let bs = map bIdent $ depOrderedBindings s
  -- fix function and argument types
  ps <- mapM initiateArgumentTypes bs
  -- typecheck the bindings
  mapM_ typeCheckDefinition $ zip bs ps
  -- type-check sections
  mapM_ (inferType TTSL . snd) sections

-----------------------------------------------------------------------------

-- | Returns the list of bindings, which is ordered accoring to the
-- respective dependencies of the entries.
depOrderedBindings ::
  Specification -> [Binding Id]
depOrderedBindings Specification {..} =
  let -- get the definition ids
      xs :: [Id]
      xs = map bIdent definitions
      -- get Id map
      im :: IntMap (Binding Int)
      im = fromList $ zip xs definitions
      -- create list of edges
      es :: [(Id, Id)]
      es = concatMap (\x -> map (x,) $ deps xs x) xs
      -- create the depencency graph
      g :: Graph
      g = buildG (minimum xs, maximum xs) es
   in case xs of
        [] -> []
        _ ->
          map (\i -> assert (member i im) (im ! i)) $
            filter (`elem` xs) $
              topSort $
                transposeG g
  where
    deps ::
      [Int] -> Int -> [Int]

    deps xs x =
      S.toList $
        S.intersection
          (S.fromList $ assert (member x dependencies) (dependencies ! x))
          (S.fromList xs)

-----------------------------------------------------------------------------

initiateArgumentTypes ::
  Id -> TypeCheck Id
initiateArgumentTypes i = do
  as <- gets ((\m -> assert (member i m) (m ! i)) . arguments . spec)

  case as of
    [] -> return i
    _ ->
      newP >>= \case
        TPoly j -> do
          void $ update i $ buildArgType j as
          return j
        _ -> assert False undefined
  where
    buildArgType j = \case
      (x : xr) -> TFml (TPoly x) $ buildArgType j xr
      [] -> TPoly j

-----------------------------------------------------------------------------

typeCheckDefinition ::
  (Id, Id) -> TypeCheck ()
typeCheckDefinition (i, j) =
  gets ((\m -> assert (member i m) (m ! i)) . bindings . spec) >>= \case
    GuardedBinding xs ->
      foldM_
        (\t x -> inferType t x >> lkType (exprId x))
        (TPoly j)
        xs
    _ -> assert False undefined

-----------------------------------------------------------------------------

inferType ::
  ExprType -> Expression -> TypeCheck ()
inferType t e = case expr e of
  BaseTrue -> checkExprType e t TBoolean
  BaseFalse -> checkExprType e t TBoolean
  BaseWild -> checkExprType e t TPattern
  BaseOtherwise -> checkExprType e t TBoolean
  BaseCon {} -> checkExprType e t TNumber
  NumSMin x -> inferE1 e t TNumber x $ TSet t
  NumSMax x -> inferE1 e t TNumber x $ TSet t
  NumSSize x -> newP >>= \p -> inferE1 e t TNumber x $ TSet p
  NumPlus x y -> inferE2 e t TNumber x t y t
  NumRPlus xs x -> inferEI e t TNumber xs x t
  NumMinus x y -> inferE2 e t TNumber x t y t
  NumMul x y -> inferE2 e t TNumber x t y t
  NumRMul xs x -> inferEI e t TNumber xs x t
  NumDiv x y -> inferE2 e t TNumber x t y t
  NumMod x y -> inferE2 e t TNumber x t y t
  SetExplicit [] -> newP >>= \p -> checkExprType e t $ TSet p
  SetExplicit xs -> newP >>= \p -> inferEX e t (TSet p) xs p
  SetRange x y z -> inferE3 e t (TSet TNumber) x TNumber y TNumber z TNumber
  SetCup x y -> newP >>= \p -> inferE2 e t (TSet p) x t y t
  SetCap x y -> newP >>= \p -> inferE2 e t (TSet p) x t y t
  SetMinus x y -> newP >>= \p -> inferE2 e t (TSet p) x t y t
  SetRCap xs x -> newP >>= \p -> inferEI e t (TSet p) xs x t
  SetRCup xs x -> newP >>= \p -> inferEI e t (TSet p) xs x t
  BlnEQ x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnNEQ x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnGE x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnGEQ x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnLE x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnLEQ x y -> inferE2 e t TBoolean x TNumber y TNumber
  BlnElem x y -> newP >>= \p -> inferE2 e t TBoolean x p y (TSet p)
  BlnNot x -> inferE1 e t TBoolean x t
  BlnOr x y -> inferE2 e t TBoolean x t y t
  BlnROr xs x -> inferEI e t TBoolean xs x t
  BlnAnd x y -> inferE2 e t TBoolean x t y t
  BlnRAnd xs x -> inferEI e t TBoolean xs x t
  BlnImpl x y -> inferE2 e t TBoolean x t y t
  BlnEquiv x y -> inferE2 e t TBoolean x t y t
  TslNext x -> inferE1 e t TTSL x t
  TslRNext x y -> inferE2 e t TTSL x TNumber y t
  TslPrevious x -> inferE1 e t TTSL x t
  TslRPrevious x y -> inferE2 e t TTSL x TNumber y t
  TslGlobally x -> inferE1 e t TTSL x t
  TslRGlobally x y -> inferER e t x y
  TslFinally x -> inferE1 e t TTSL x t
  TslRFinally x y -> inferER e t x y
  TslHistorically x -> inferE1 e t TTSL x t
  TslRHistorically x y -> inferER e t x y
  TslOnce x -> inferE1 e t TTSL x t
  TslROnce x y -> inferER e t x y
  TslUntil x y -> inferE2 e t TTSL x t y t
  TslWeak x y -> inferE2 e t TTSL x t y t
  TslAsSoonAs x y -> inferE2 e t TTSL x t y t
  TslRelease x y -> inferE2 e t TTSL x t y t
  TslSince x y -> inferE2 e t TTSL x t y t
  TslTriggered x y -> inferE2 e t TTSL x t y t
  Pattern x y -> inferE2 e t TBoolean x TTSL y TPattern
  Colon x y -> do
    -- check guard type
    inferType TBoolean x
    -- check expression type
    inferType t y
    -- get possibly updated result
    t' <- lkType $ exprId y
    -- update type of the colon expression
    checkExprType e t t'
  BaseUpd x i -> do
    -- every update expression must be part of a TSL formula
    checkExprType e t TTSL
    -- get the current type of the assigned output signal
    t' <-
      lkType i >>= \case
        TSignal t'' -> return $ TSignal t''
        TPoly i' -> do
          p <- newP
          update i' $ TSignal p
          return $ TSignal p
        t'' -> do
          p <- newP
          errExpect (TSignal p) t'' $ srcPos e
    -- infer the type of the assigned term
    inferType t' x
  BaseFn x y -> do
    -- create fresh dummy type
    p <- newP
    -- enforce formula type on the applied term
    inferType (TFml p t) x
    -- the result must still be a formula type
    lkType (exprId x) >>= \case
      TFml a b -> do
        -- infer the argument type
        inferType a y
        -- update the type of the expression
        updExprType b e
      _ -> assert False undefined
  BaseConFn i ->
    lkType i >>= \case
      TSignal t' -> checkExprType e t $ TSignal t'
      TPoly i -> do
        p <- newP
        update i $ TSignal p
        checkExprType e t $ TSignal p
      t -> do
        p <- newP
        errExpect (TSignal p) t $ srcPos e
  BaseId i ->
    gets (IM.lookup i . bindings . spec) >>= \case
      Nothing -> do
        t' <- lkType i
        tR <- resolve t

        gets (IM.lookup i . scopes . spec) >>= \case
          Nothing
            | tslFn tR -> do
                tS <- liftToSignalLevel tR
                checkExprType e tS t'
            | otherwise -> case tR of
                TTSL -> checkExprType e (TSignal TBoolean) t'
                TBoolean -> checkExprType e (TSignal TBoolean) t'
                TPoly j -> do
                  p <- newP
                  update j $ TSignal p
                  checkExprType e (TSignal p) t'
                _ -> checkExprType e tR t'
          Just () -> checkExprType e tR t'
      Just b -> case b of
        GuardedBinding _ -> do
          as <- gets ((\m -> assert (member i m) (m ! i)) . arguments . spec)
          ts <- mapM lkType as
          tf <- lkType i
          checkExprType e t tf
          t' <- lkType $ exprId e
          -- reset function types
          mapM_ (uncurry update) $ zip as ts
          update i t'
        PatternBinding x y -> do
          inferType TTSL x
          inferType TPattern y
          lkType i >>= checkExprType e TTSL
        SetBinding s -> do
          inferType (TSet t) s
          lkType (exprId s) >>= \case
            TSet t -> lkType i >>= checkExprType e t
            _ -> assert False undefined
        RangeBinding x _ y _ -> do
          inferType TNumber x
          inferType TNumber y
          lkType i >>= checkExprType e TNumber
  where
    inferE1 e t t' x tx = do
      checkExprType e t t'
      inferType tx x

    inferE2 e t t' x tx y ty = do
      inferE1 e t t' x tx
      inferType ty y

    inferE3 e t t' x tx y ty z tz = do
      inferE2 e t t' x tx y ty
      inferType tz z

    inferEI e t t' xs x tx = do
      checkExprType e t t'
      mapM_ inferBind xs
      inferType tx x

    inferEX e t t' xs txs = do
      checkExprType e t t'
      mapM_ (inferType txs) xs

    inferER e t x y = do
      checkExprType e t TTSL
      case expr x of
        Colon x' y' -> do
          inferType TNumber x'
          inferType TNumber y'
          inferType TTSL y
        _ -> do
          p <- newP
          inferType p x
          t' <- lkType $ exprId x
          errRange t' $ srcPos x

    inferBind e = case expr e of
      BlnElem x y -> case expr x of
        BaseId i -> do
          t <- lkType i
          inferType (TSet t) y
          updExprType t x
        _ -> assert False undefined
      BlnLE e' z -> do
        inferType TNumber z
        case expr e' of
          BlnLE x y -> do
            inferType TNumber x
            inferType TNumber y
          BlnLEQ x y -> do
            inferType TNumber x
            inferType TNumber y
          _ -> assert False undefined
      BlnLEQ e' z -> do
        inferType TNumber z
        case expr e' of
          BlnLE x y -> do
            inferType TNumber x
            inferType TNumber y
          BlnLEQ x y -> do
            inferType TNumber x
            inferType TNumber y
          _ -> assert False undefined
      _ -> assert False undefined

    tslFn = \case
      (TFml _ TTSL) -> True
      (TFml _ t) -> tslFn t
      _ -> False

-----------------------------------------------------------------------------

newP ::
  TypeCheck ExprType
newP = do
  s@ST {..} <- get
  put s {tCount = tCount + 1}
  return $ TPoly tCount

-----------------------------------------------------------------------------

-- | Checks consistencty of types with respect to the type enforced by the
-- parent expression, the type of the current expression, and the type
-- stored in the type table.
--
--   e:  currently evaluated expression
--   pT: type constraint of the parent expressions
--   eT: type constraint of the currently evaluated expression
checkExprType ::
  Expression -> ExprType -> ExprType -> TypeCheck ()
checkExprType e pT eT =
  vt pT eT >>= \case
    Nothing -> errExpect pT eT $ srcPos e
    Just t -> updExprType t e

-----------------------------------------------------------------------------

vt :: ExprType -> ExprType -> TypeCheck (Maybe ExprType)
-- enforced by parent  enforced by current

vt (TPoly i) t = updParent i t
vt t (TPoly j) = updCurrent j t
vt (TSignal TBoolean) TBoolean = return $ Just $ TSignal TBoolean
vt (TSignal (TPoly i)) TBoolean =
  updParent i TBoolean >>= \case
    Nothing -> return Nothing
    Just t -> return $ Just $ liftS t
vt (TSignal t) (TSignal t') =
  vt t t' >>= \case
    Nothing -> return Nothing
    Just t -> return $ Just $ liftS t
vt TBoolean TBoolean = return $ Just TBoolean
vt TBoolean TTSL = return $ Just TTSL
vt TBoolean (TSignal TBoolean) = return $ Just TTSL
vt TBoolean (TSignal (TPoly i)) =
  updCurrent i TBoolean >>= \case
    Nothing -> return Nothing
    Just _ -> return $ Just TTSL
vt TTSL TTSL = return $ Just TTSL
vt TTSL TBoolean = return $ Just TTSL
vt TTSL (TSignal TBoolean) = return $ Just TTSL
vt TTSL (TSignal (TPoly i)) =
  updCurrent i TBoolean >>= \case
    Nothing -> return Nothing
    Just _ -> return $ Just TTSL
vt TPattern TPattern = return $ Just TPattern
vt TPattern TTSL = return $ Just TPattern
vt TPattern TBoolean = return $ Just TPattern
vt (TSet t) (TSet t') =
  vt t t' >>= \case
    Nothing -> return Nothing
    Just t -> return $ Just $ TSet t
vt TNumber TNumber = return $ Just TNumber
vt (TFml a b) (TFml a' b')
  | onSignalLevel (TFml a b) || onSignalLevel (TFml a' b') =
      liftToSignalLevel (TFml a b) >>= \case
        TFml a1 b1 ->
          liftToSignalLevel (TFml a' b') >>= \case
            TFml a2 b2 ->
              vt a1 a2 >>= \case
                Nothing -> return Nothing
                Just aT ->
                  vt b1 b2 >>= \case
                    Nothing -> return Nothing
                    Just bT -> return $ Just $ TFml aT bT
            _ -> assert False undefined
        _ -> assert False undefined
  | otherwise =
      vt a a' >>= \case
        Nothing -> return Nothing
        Just aT ->
          vt b b' >>= \case
            Nothing -> return Nothing
            Just bT -> return $ Just $ TFml aT bT
vt _ _ = return Nothing

-----------------------------------------------------------------------------

updExprType ::
  ExprType -> Expression -> TypeCheck ()
updExprType t Expr {..} =
  modify $ \st -> st {tTypes = insert exprId t $ tTypes st}

-----------------------------------------------------------------------------

-- | Updates a poly parent type by the given expression type.
updParent ::
  Int -> ExprType -> TypeCheck (Maybe ExprType)
updParent i = \case
  -- join poly types
  TPoly j ->
    lkType j >>= \case
      TPoly j' ->
        lkType i >>= \case
          TPoly i'
            | i' == j' -> return $ Just $ TPoly j
            | i' < j' -> update j' (TPoly i') >> return (Just (TPoly i'))
            | i' > j' -> update i' (TPoly j') >> return (Just (TPoly j'))
          t -> update j' t >> return (Just t)
      t -> updParent i t
  -- join poly types and lift them to the signal level
  TSignal (TPoly j) ->
    lkType j >>= \case
      TPoly j' ->
        lkType i >>= \case
          TPoly i' -> do
            update i' $ TSignal $ TPoly j'
            return $ Just $ TSignal $ TPoly j'
          TSignal t ->
            vt (TSignal t) (TSignal (TPoly j')) >>= \case
              Nothing -> return Nothing
              Just t' -> do
                update i t'
                return $ Just t'
          t ->
            vt t (TSignal (TPoly j')) >>= \case
              Nothing -> return Nothing
              Just t' -> do
                update i t'
                return $ Just t'
      t -> return $ Just $ liftS t
  -- set the fixed type
  t ->
    lkType i >>= \case
      TPoly i' -> update i' t >> return (Just t)
      t' ->
        vt t' t >>= \case
          Nothing -> return Nothing
          Just t'' -> do
            update i t''
            return $ Just t''

-----------------------------------------------------------------------------

-- | Updates a given poly type by the expression type of the parent.
updCurrent ::
  Int -> ExprType -> TypeCheck (Maybe ExprType)
updCurrent i = \case
  -- join poly types
  TPoly j ->
    lkType j >>= \case
      TPoly j' ->
        lkType i >>= \case
          TPoly i'
            | i' == j' -> return $ Just $ TPoly j
            | i' < j' -> update j' (TPoly i') >> return (Just (TPoly i'))
            | i' > j' -> update i' (TPoly j') >> return (Just (TPoly j'))
          t -> update j' t >> return (Just t)
      t -> updCurrent i t
  -- join poly types and lift them to the signal level
  TSignal (TPoly j) ->
    lkType j >>= \case
      TPoly j' ->
        lkType i >>= \case
          TPoly i' -> do
            update i' $ TSignal $ TPoly j'
            return $ Just $ TSignal $ TPoly j'
          TSignal t ->
            vt (TSignal (TPoly j')) (TSignal t) >>= \case
              Nothing -> return Nothing
              Just t' -> do
                update i t'
                return $ Just t'
          t ->
            vt (TSignal (TPoly j')) t >>= \case
              Nothing -> return Nothing
              Just t' -> do
                update i t'
                return $ Just t'
      t -> return $ Just $ liftS t
  -- set the fixed type
  t ->
    lkType i >>= \case
      TPoly i' -> update i' t >> return (Just t)
      t' ->
        vt t t' >>= \case
          Nothing -> return Nothing
          Just t'' -> do
            update i t''
            return $ Just t''

-----------------------------------------------------------------------------

-- | Updates the expression type of the given id.
update ::
  Id -> ExprType -> TypeCheck ()
update i t =
  gets (IM.lookup i . tTypes) >>= \case
    Just (TPoly j)
      | i == j -> modify $ \st -> st {tTypes = insert i t $ tTypes st}
      | otherwise -> assert (j < i) $ update j t
    _ -> modify $ \st -> st {tTypes = insert i t $ tTypes st}

-----------------------------------------------------------------------------

-- | Lifts a type to the signal level under the assumption that every
-- poly is already an inner signal type.
liftS ::
  ExprType -> ExprType
liftS = \case
  TFml x y -> TFml (liftS x) (liftS' y)
  TSignal x -> TSignal x
  x -> TSignal x
  where
    liftS' = \case
      TSignal x -> TSignal x
      TFml x y -> TFml (liftS x) (liftS' y)
      TTSL -> TSignal TBoolean
      x -> TSignal x

-----------------------------------------------------------------------------

-- | Lifts a type to the signal level.
liftToSignalLevel ::
  ExprType -> TypeCheck ExprType
liftToSignalLevel = \case
  TPoly i ->
    lkType i >>= \case
      TPoly j -> poly j
      t -> liftToSignalLevel t
  TFml x y -> liftToSignalLevel' (TFml x y)
  x -> return $ liftS x
  where
    liftToSignalLevel' = \case
      TPoly i ->
        lkType i >>= \case
          TPoly j -> poly j
          t -> liftToSignalLevel' t
      TFml x y -> do
        x' <- liftToSignalLevel x
        y' <- liftToSignalLevel' y
        return $ TFml x' y'
      TTSL -> return $ TSignal TBoolean
      x -> return $ liftS x

    poly j = do
      p <- newP
      update j $ TSignal p
      return $ TSignal p

-----------------------------------------------------------------------------

-- | Checks, whether a type is on the signal level.
onSignalLevel ::
  ExprType -> Bool
onSignalLevel = \case
  TSignal {} -> True
  TFml x y -> onSignalLevel y || onSignalLevel x
  _ -> False

-----------------------------------------------------------------------------

-- | Resolves all poly-type-pointers to their final target
resolve ::
  ExprType -> TypeCheck ExprType
resolve = \case
  TSet t -> TSet <$> resolve t
  TFml t t' -> fml t t'
  TPoly i ->
    gets (IM.lookup i . tTypes) >>= \case
      Nothing -> return $ TPoly i
      Just (TSet t) -> TSet <$> resolve t
      Just (TFml t t') -> fml t t'
      Just (TPoly j)
        | i == j -> return $ TPoly i
        | otherwise -> assert (j < i) $ resolve $ TPoly j
      Just (TSignal t) -> liftS <$> resolve t
      Just t -> return t
  TSignal t -> liftS <$> resolve t
  t -> return t
  where
    fml t t' = do
      t1 <- resolve t
      t2 <- resolve t'
      let tf = TFml t1 t2
      if
          | onSignalLevel tf -> liftToSignalLevel tf
          | otherwise -> return tf

-----------------------------------------------------------------------------

lkType ::
  Int -> TypeCheck ExprType
lkType =
  resolve . TPoly

-----------------------------------------------------------------------------

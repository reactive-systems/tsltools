-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Eval
-- Maintainer  :  Felix Klein
--
-- Evaluation function for internal variables.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , ViewPatterns
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Eval
  ( eval
  ) where

-----------------------------------------------------------------------------

import TSL.Logic
  ( SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , Formula(..)
  )

import TSL.Types
  ( ExprType(..)
  )

import TSL.SymbolTable
  ( SymbolTable(..)
  , Kind(..)
  , stKind
  , stArgs
  , stType
  , stBindings
  )

import TSL.Binding
  ( BoundExpr(..)
  )

import Control.Exception
  ( assert
  )

import TSL.Expression
  ( Expression
  , Expr(..)
  , Expr'(..)
  , ExprPos
  )

import TSL.Error
  ( Error
  , runtimeError
  )

import Data.Array
  ( bounds
  )

import Data.Array.ST
  ( STArray
  , newArray
  , writeArray
  , readArray
  )

import Control.Monad.ST
  ( ST
  , runST
  )

import Control.Monad
  ( foldM
  , liftM2
  , liftM3
  )

import Data.Set
  ( Set
  , member
  , toList
  , fromList
  , union
  , intersection
  , difference
  , size
  )

-----------------------------------------------------------------------------

-- | Evaluation result.

data Value =
    VEmpty
  | VWildcard
  | VBoolean Bool
  | VInt Int
  | VSet (Set Value)
  | VTSL (Formula Int)
  | VSTerm (SignalTerm Int)
  | VFTerm (FunctionTerm Int)
  | VPTerm (PredicateTerm Int)
  | VError ExprPos String
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------

eval
  :: SymbolTable -> [Expression] -> Either Error [Formula Int]


eval s@SymbolTable{..} es = runST $ do
  a <- newArray (bounds symtable) VEmpty
  sequence <$> mapM (evalE a) es

  where
    evalE a e =
      evalExpression s a e >>= \case
        VPTerm pt      -> return $ Right $ Check pt
        VTSL fml       -> return $ Right fml
        VBoolean True  -> return $ Right TTrue
        VBoolean False -> return $ Right FFalse
        VError p str   -> return $ runtimeError p str
        _              -> assert False undefined

-----------------------------------------------------------------------------

evalExpression
  :: SymbolTable -> STArray s Int Value -> Expression -> ST s Value

evalExpression s a e =
  errPos (srcPos e) <$> case expr e of
    BaseWild             -> return VWildcard
    BaseTrue             -> return $ VBoolean True
    BaseFalse            -> return $ VBoolean False
    BaseOtherwise        -> return $ VBoolean True
    BaseCon c            -> return $ VInt c
    SetExplicit xs       -> VSet . fromList <$> mapM evalE xs
    NumSMin x            -> vMinS <$> evalE x
    NumSMax x            -> vMaxS <$> evalE x
    NumSSize x           -> vSize <$> evalE x
    BlnNot x             -> vNot <$> evalE x
    TslNext x            -> vNext <$> evalE x
    TslPrevious x        -> vPrevious <$> evalE x
    TslGlobally x        -> vGlobally <$> evalE x
    TslFinally x         -> vFinally <$> evalE x
    TslHistorically x    -> vHistorically <$> evalE x
    TslOnce x            -> vOnce <$> evalE x
    NumPlus x y          -> liftM2 vPlus (evalE x) $ evalE y
    NumMinus x y         -> liftM2 vMinus (evalE x) $ evalE y
    NumMul x y           -> liftM2 vMul (evalE x) $ evalE y
    NumDiv x y           -> liftM2 vDiv (evalE x) $ evalE y
    NumMod x y           -> liftM2 vMod (evalE x) $ evalE y
    SetCup x y           -> liftM2 vCup (evalE x) $ evalE y
    SetCap x y           -> liftM2 vCap (evalE x) $ evalE y
    SetMinus x y         -> liftM2 vDif (evalE x) $ evalE y
    BlnEQ x y            -> liftM2 vEQ (evalE x) $ evalE y
    BlnNEQ x y           -> liftM2 vNEQ (evalE x) $ evalE y
    BlnGE x y            -> liftM2 vGE (evalE x) $ evalE y
    BlnGEQ x y           -> liftM2 vGEQ (evalE x) $ evalE y
    BlnLE x y            -> liftM2 vLE (evalE x) $ evalE y
    BlnLEQ x y           -> liftM2 vLEQ (evalE x) $ evalE y
    BlnElem x y          -> liftM2 vElem (evalE x) $ evalE y
    BlnAnd x y           -> liftM2 vAnd (evalE x) $ evalE y
    BlnOr x y            -> liftM2 vOr (evalE x) $ evalE y
    BlnImpl x y          -> liftM2 vImpl (evalE x) $ evalE y
    BlnEquiv x y         -> liftM2 vEquiv (evalE x) $ evalE y
    TslUntil x y         -> liftM2 vUntil (evalE x) $ evalE y
    TslWeak x y          -> liftM2 vWeak (evalE x) $ evalE y
    TslAsSoonAs x y      -> liftM2 vAsSoonAs (evalE x) $ evalE y
    TslRelease x y       -> liftM2 vRelease (evalE x) $ evalE y
    TslSince x y         -> liftM2 vSince (evalE x) $ evalE y
    TslTriggered x y     -> liftM2 vTriggered (evalE x) $ evalE y
    SetRange x y z       -> liftM3 vRange (evalE x) (evalE y) $ evalE z
    NumRPlus xs x        -> evalRangeExpr vPlus x xs
    NumRMul xs x         -> evalRangeExpr vMul x xs
    SetRCup xs x         -> evalRangeExpr vCup x xs
    SetRCap xs x         -> evalRangeExpr vCap x xs
    BlnROr xs x          -> evalRangeExpr vOr x xs
    BlnRAnd xs x         -> evalRangeExpr vAnd x xs
    TslRNext x y         -> liftM2 vNextN (evalE x) $ evalE y
    TslRPrevious x y     -> liftM2 vPreviousN (evalE x) $ evalE y
    TslRGlobally x y     -> case expr x of
      Colon n m -> liftM3 vGloballyR (evalE n) (evalE m) $ evalE y
      _         -> assert False undefined
    TslRFinally x y      -> case expr x of
      Colon n m -> liftM3 vFinallyR (evalE n) (evalE m) $ evalE y
      _         -> assert False undefined
    TslRHistorically x y -> case expr x of
      Colon n m -> liftM3 vHistoricallyR (evalE n) (evalE m) $ evalE y
      _         -> assert False undefined
    TslROnce x y         -> case expr x of
      Colon n m -> liftM3 vOnceR (evalE n) (evalE m) $ evalE y
      _         -> assert False undefined
    BaseUpd x i          ->
      evalE x >>= \case
        VSTerm x'      -> return $ VTSL $ Update i x'
        VFTerm x'      -> return $ VTSL $ Update i $ FunctionTerm x'
        VPTerm x'      -> return $ VTSL $ Update i $ PredicateTerm x'
        VBoolean True  -> return $ VTSL $ Update i $ PredicateTerm BooleanTrue
        VBoolean False -> return $ VTSL $ Update i $ PredicateTerm BooleanFalse
        _              -> assert False undefined
    BaseConFn i          -> case stType s i of
      TSignal (TPoly _) -> return $ VFTerm $ FunctionSymbol i
      TSignal TBoolean  -> return $ VPTerm $ PredicateSymbol i
      _                 -> assert False undefined
    BaseId i             -> case stKind s i of
      Input     -> case stType s i of
        TSignal TBoolean -> return $ VPTerm $ BooleanInput i
        _                -> return $ VSTerm $ Signal i
      Output    -> return $ VSTerm $ Signal i
      Constant  -> case stType s i of
        TSignal (TPoly _) -> return $ VFTerm $ FunctionSymbol i
        TSignal TBoolean  -> return $ VPTerm $ PredicateSymbol i
        _                 -> assert False undefined
      Function  -> return $ VFTerm $ FunctionSymbol i
      Predicate -> return $ VPTerm $ PredicateSymbol i
      Internal  -> readArray a i >>= \case
        VEmpty ->
          assert (null $ stArgs s i) $
          case stBindings s i of
            Just (GuardedBinding xs) -> vFirst xs
            _                        -> assert False undefined
        v      -> return v
    BaseFn {}            -> evalFnApplication [] e
    Pattern {}           -> assert False undefined
    Colon {}             -> assert False undefined

  where
    evalE = evalExpression s a

    evalFnApplication xs x = case expr x of
      BaseFn y z -> do
        z' <- evalE z
        case expr y of
          BaseFn {} -> evalFnApplication (z':xs) y
          BaseId i  -> case stKind s i of
            Constant  -> assert (null xs) $ case stType s i of
              TSignal (TPoly _) -> return $ VFTerm $ FunctionSymbol i
              TSignal TBoolean  -> return $ VPTerm $ PredicateSymbol i
              _                 -> assert False undefined
            Predicate ->
              return $ VPTerm $ foldl PApplied (PredicateSymbol i) $
                map signal (z':xs)
            Function  ->
              return $ VFTerm $ foldl FApplied (FunctionSymbol i) $
                map signal (z':xs)
            Internal  -> do
              let
                as = stArgs s i
                zs = assert (length (z':xs) == length as) $
                       zip as $ z':xs
              mapM_ (uncurry (writeArray a)) zs
              case stBindings s i of
                Just (GuardedBinding es) -> vFirst es
                _                        -> assert False undefined
            _         -> assert False undefined
          _         -> assert False undefined
      _          -> assert False undefined

    errPos p = \case
      VError _ str -> VError p str
      x            -> x

    signal = \case
      VSTerm x -> x
      VPTerm x -> PredicateTerm x
      VFTerm x -> FunctionTerm x
      _        -> assert False undefined

    vFirst = \case
      []     -> assert False undefined
      (e:er) -> case expr e of
        Colon x y -> evalExpression s a x >>= \case
          VBoolean True  -> evalExpression s a y
          VBoolean False -> vFirst er
          _              -> assert False undefined
        _         -> evalExpression s a e

    evalRangeExpr op u = \case
      []     -> evalE u
      (x:xr) -> case expr x of
        BlnElem y ys -> case expr y of
          BaseId i -> evalE ys >>= evalRange op u xr i
          _        -> assert False undefined
        BlnLE y m    -> case expr y of
          BlnLE n z  -> case expr z of
            BaseId i -> do
              n' <- vPlus (VInt 1) <$> evalE n
              m' <- flip vMinus (VInt 1) <$> evalE m
              evalRange op u xr i $ vRange n' (vPlus n' (VInt 1)) m'
            _        -> assert False undefined
          BlnLEQ n z -> case expr z of
            BaseId i -> do
              n' <- evalE n
              m' <- flip vMinus (VInt 1) <$> evalE m
              evalRange op u xr i $ vRange n' (vPlus n' (VInt 1)) m'
            _        -> assert False undefined
          _          -> assert False undefined
        BlnLEQ y m   -> case expr y of
          BlnLE n z  -> case expr z of
            BaseId i -> do
              n' <- vPlus (VInt 1) <$> evalE n
              m' <- evalE m
              evalRange op u xr i $ vRange n' (vPlus n' (VInt 1)) m'
            _        -> assert False undefined
          BlnLEQ n z -> case expr z of
            BaseId i -> do
              n' <- evalE n
              m' <- evalE m
              evalRange op u xr i $ vRange n' (vPlus n' (VInt 1)) m'
            _        -> assert False undefined
          _          -> assert False undefined
        _            -> assert False undefined

    evalRange op u xr i = \case
      VSet (toList -> [])   -> return $ VError undefined "Empty range"
      VSet (toList -> v:vr) -> do
        writeArray a i v
        r <- evalRangeExpr op u xr
        foldM
          (\b v -> do
              writeArray a i v
              op b <$> evalRangeExpr op u xr
          ) r vr
      _ -> assert False undefined

-----------------------------------------------------------------------------

vMin
  :: Value -> Value -> Value

vMin (VInt x) (VInt y) = VInt $ min x y
vMin _        _        = assert False undefined

-----------------------------------------------------------------------------

vMinS
  :: Value -> Value

vMinS (VSet (toList -> x:xr)) = foldl vMin x xr
vMinS (VSet (toList -> []))   = VError undefined "Minimum of empty set"
vMinS _                      = assert False undefined

-----------------------------------------------------------------------------

vMax
  :: Value -> Value -> Value

vMax (VInt x) (VInt y) = VInt $ max x y
vMax _        _        = assert False undefined

-----------------------------------------------------------------------------

vMaxS
  :: Value -> Value

vMaxS (VSet (toList -> x:xr)) = foldl vMax x xr
vMaxS (VSet (toList -> []))   = VError undefined "Maximum of empty set"
vMaxS _                      = assert False undefined

-----------------------------------------------------------------------------

vElem
  :: Value -> Value -> Value

vElem x (VSet y) = VBoolean $ member x y
vElem _ _        = assert False undefined

-----------------------------------------------------------------------------

vPlus
  :: Value -> Value -> Value

vPlus (VInt x) (VInt y) = VInt $ x + y
vPlus _        _        = assert False undefined

-----------------------------------------------------------------------------

vMinus
  :: Value -> Value -> Value

vMinus (VInt x) (VInt y) = VInt $ x - y
vMinus _        _        = assert False undefined

-----------------------------------------------------------------------------

vDiv
  :: Value -> Value -> Value

vDiv (VInt x) (VInt y) = VInt $ x `div` y
vDiv _        _        = assert False undefined

-----------------------------------------------------------------------------

vMod
  :: Value -> Value -> Value

vMod (VInt x) (VInt y) = VInt $ x `mod` y
vMod _        _        = assert False undefined

-----------------------------------------------------------------------------

vMul
  :: Value -> Value -> Value

vMul (VInt x) (VInt y) = VInt $ x * y
vMul _        _        = assert False undefined

-----------------------------------------------------------------------------

vCup
  :: Value -> Value -> Value

vCup (VSet x) (VSet y) = VSet $ union x y
vCup _        _        = assert False undefined

-----------------------------------------------------------------------------

vCap
  :: Value -> Value -> Value

vCap (VSet x) (VSet y) = VSet $ intersection x y
vCap _        _        = assert False undefined

-----------------------------------------------------------------------------

vDif
  :: Value -> Value -> Value

vDif (VSet x) (VSet y) = VSet $ difference x y
vDif _        _        = assert False undefined

-----------------------------------------------------------------------------

vSize
  :: Value -> Value

vSize (VSet x) = VInt $ size x
vSize _        = assert False undefined

-----------------------------------------------------------------------------

vRange
  :: Value -> Value -> Value -> Value

vRange (VInt x) (VInt y) (VInt z) = VSet $ fromList $ map VInt [x, y .. z]
vRange _        _        _        = assert False undefined

-----------------------------------------------------------------------------

vEQ
  :: Value -> Value -> Value

vEQ (VInt x) (VInt y) = VBoolean $ x == y
vEQ _        _        = assert False undefined

-----------------------------------------------------------------------------

vNEQ
  :: Value -> Value -> Value

vNEQ (VInt x) (VInt y) = VBoolean $ x /= y
vNEQ _        _        = assert False undefined

-----------------------------------------------------------------------------

vLE
  :: Value -> Value -> Value

vLE (VInt x) (VInt y) = VBoolean $ x < y
vLE _        _        = assert False undefined

-----------------------------------------------------------------------------

vLEQ
  :: Value -> Value -> Value

vLEQ (VInt x) (VInt y) = VBoolean $ x <= y
vLEQ _        _        = assert False undefined

-----------------------------------------------------------------------------

vGE
  :: Value -> Value -> Value

vGE (VInt x) (VInt y) = VBoolean $ x > y
vGE _        _        = assert False undefined

-----------------------------------------------------------------------------

vGEQ
  :: Value -> Value -> Value

vGEQ (VInt x) (VInt y) = VBoolean $ x >= y
vGEQ _        _        = assert False undefined

-----------------------------------------------------------------------------

vNot
  :: Value -> Value

vNot (VBoolean True)     = VBoolean False
vNot (VBoolean False)    = VBoolean True
vNot (VPTerm x)          = VTSL $ Not $ Check x
vNot (VSTerm (Signal x)) = VTSL $ Not $ cb x
vNot (VTSL x)            = VTSL $ Not x
vNot _                   = assert False undefined

-----------------------------------------------------------------------------

vOr
  :: Value -> Value -> Value

vOr (VBoolean True)     _                   = VBoolean True
vOr _                   (VBoolean True)     = VBoolean True
vOr (VBoolean False)    (VBoolean y)        = VBoolean y
vOr (VBoolean False)    (VPTerm y)          = VTSL $ Check y
vOr (VBoolean False)    (VSTerm (Signal y)) = VTSL $ Check $ BooleanInput y
vOr (VBoolean False)    (VTSL y)            = VTSL y
vOr (VPTerm x)          (VBoolean False)    = VTSL $ Check x
vOr (VSTerm (Signal x)) (VBoolean False)    = VTSL $ Check $ BooleanInput x
vOr (VTSL x)            (VBoolean False)    = VTSL x
vOr (VTSL x)            (VTSL y)            = VTSL $ Or [x, y]
vOr (VPTerm x)          (VTSL y)            = VTSL $ Or [Check x, y]
vOr (VSTerm (Signal x)) (VTSL y)            = VTSL $ Or [cb x, y]
vOr (VTSL x)            (VPTerm y)          = VTSL $ Or [x, Check y]
vOr (VPTerm x)          (VPTerm y)          = VTSL $ Or [Check x, Check y]
vOr (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Or [cb x, Check y]
vOr (VTSL x)            (VSTerm (Signal y)) = VTSL $ Or [x, cb y]
vOr (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Or [Check x, cb y]
vOr (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Or [cb x, cb y]
vOr _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vAnd
  :: Value -> Value -> Value

vAnd (VBoolean False)    _                   = VBoolean False
vAnd _                   (VBoolean False)    = VBoolean False
vAnd (VBoolean True)     (VBoolean y)        = VBoolean y
vAnd (VBoolean True)     (VPTerm y)          = VTSL $ Check y
vAnd (VBoolean True)     (VSTerm (Signal y)) = VTSL $ cb y
vAnd (VBoolean True)     (VTSL y)            = VTSL y
vAnd (VPTerm x)          (VBoolean True)     = VTSL $ Check x
vAnd (VSTerm (Signal x)) (VBoolean True)     = VTSL $ cb x
vAnd (VTSL x)            (VBoolean True)     = VTSL x
vAnd (VTSL x)            (VTSL y)            = VTSL $ And [x, y]
vAnd (VPTerm x)          (VTSL y)            = VTSL $ And [Check x, y]
vAnd (VSTerm (Signal x)) (VTSL y)            = VTSL $ And [cb x, y]
vAnd (VTSL x)            (VPTerm y)          = VTSL $ And [x, Check y]
vAnd (VPTerm x)          (VPTerm y)          = VTSL $ And [Check x, Check y]
vAnd (VSTerm (Signal x)) (VPTerm y)          = VTSL $ And [cb x, Check y]
vAnd (VTSL x)            (VSTerm (Signal y)) = VTSL $ And [x, cb y]
vAnd (VPTerm x)          (VSTerm (Signal y)) = VTSL $ And [Check x, cb y]
vAnd (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ And [cb x, cb y]
vAnd _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vImpl
  :: Value -> Value -> Value

vImpl (VBoolean True)     (VBoolean False)    = VBoolean False
vImpl (VBoolean True)     (VPTerm y)          = VTSL $ Check y
vImpl (VBoolean True)     (VSTerm (Signal y)) = VTSL $ cb y
vImpl (VBoolean True)     (VTSL y)            = VTSL y
vImpl _                   (VBoolean True)     = VBoolean True
vImpl (VBoolean False)     _                  = VBoolean True
vImpl (VPTerm x)          (VBoolean False)    = VTSL $ Not $ Check x
vImpl (VSTerm (Signal x)) (VBoolean False)    = VTSL $ Not $ cb x
vImpl (VTSL x)            (VBoolean False)    = VTSL $ Not x
vImpl (VTSL x)            (VTSL y)            = VTSL $ Implies x y
vImpl (VPTerm x)          (VTSL y)            = VTSL $ Implies (Check x) y
vImpl (VSTerm (Signal x)) (VTSL y)            = VTSL $ Implies (cb x) y
vImpl (VTSL x)            (VPTerm y)          = VTSL $ Implies x $ Check y
vImpl (VPTerm x)          (VPTerm y)          = VTSL $ Implies (Check x) $ Check y
vImpl (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Implies (cb x) $ Check y
vImpl (VTSL x)            (VSTerm (Signal y)) = VTSL $ Implies x $ cb y
vImpl (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Implies (Check x) $ cb y
vImpl (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Implies (cb x) $ cb y
vImpl _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vEquiv
  :: Value -> Value -> Value

vEquiv (VBoolean True)     (VBoolean True)     = VBoolean True
vEquiv (VBoolean True)     (VBoolean False)    = VBoolean False
vEquiv (VBoolean True)     (VPTerm y)          = VTSL $ Check y
vEquiv (VBoolean True)     (VSTerm (Signal y)) = VTSL $ cb y
vEquiv (VBoolean True)     (VTSL y)            = VTSL y
vEquiv (VBoolean False)    (VBoolean True)     = VBoolean False
vEquiv (VBoolean False)    (VBoolean False)    = VBoolean True
vEquiv (VBoolean False)    (VPTerm y)          = VTSL $ Not $ Check y
vEquiv (VBoolean False)    (VSTerm (Signal y)) = VTSL $ Not $ cb y
vEquiv (VBoolean False)    (VTSL y)            = VTSL $ Not y
vEquiv (VTSL x)            (VTSL y)            = VTSL $ Equiv x y
vEquiv (VPTerm x)          (VTSL y)            = VTSL $ Equiv (Check x) y
vEquiv (VSTerm (Signal x)) (VTSL y)            = VTSL $ Equiv (cb x) y
vEquiv (VTSL x)            (VPTerm y)          = VTSL $ Equiv x $ Check y
vEquiv (VPTerm x)          (VPTerm y)          = VTSL $ Equiv (Check x) $ Check y
vEquiv (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Equiv (cb x) $ Check y
vEquiv (VTSL x)            (VSTerm (Signal y)) = VTSL $ Equiv x $ cb y
vEquiv (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Equiv (Check x) $ cb y
vEquiv (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Equiv (cb x) $ cb y
vEquiv _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vNext
  :: Value -> Value

vNext (VBoolean True)     = VBoolean True
vNext (VBoolean False)    = VBoolean False
vNext (VPTerm x)          = VTSL $ Next $ Check x
vNext (VSTerm (Signal x)) = VTSL $ Next $ cb x
vNext (VTSL x)            = VTSL $ Next x
vNext _                   = assert False undefined

-----------------------------------------------------------------------------

vNextN
  :: Value -> Value -> Value

vNextN _        (VBoolean True)  = VBoolean True
vNextN _        (VBoolean False) = VBoolean False
vNextN (VInt n) (VTSL x)
  | n < 0                        = VError undefined $
                                     "Negative next-operator chain [" ++
                                     show n ++ "]"
  | otherwise                    = VTSL $ (!! n) $ iterate Next x
vNextN (VInt n) (VPTerm x)
  | n < 0                        = VError undefined $
                                     "Negative next-operator chain: [" ++
                                     show n ++ "]"
  | otherwise                    = VTSL $ (!! n) $ iterate Next $ Check x
vNextN (VInt n) (VSTerm (Signal x))
  | n < 0                        = VError undefined $
                                     "Negative next-operator chain: [" ++
                                     show n ++ "]"
  | otherwise                    = VTSL $ (!! n) $ iterate Next $ cb x
vNextN _        _                = assert False undefined

-----------------------------------------------------------------------------

vPrevious
  :: Value -> Value

vPrevious (VBoolean False)    = VBoolean False
vPrevious (VPTerm x)          = VTSL $ Previous $ Check x
vPrevious (VSTerm (Signal x)) = VTSL $ Previous $ cb x
vPrevious (VTSL x)            = VTSL $ Previous x
vPrevious _                   = assert False undefined

-----------------------------------------------------------------------------

vPreviousN
  :: Value -> Value -> Value

vPreviousN _        (VBoolean False) = VBoolean False
vPreviousN (VInt n) (VTSL x)
  | n < 0                            = VError undefined $
                                     "Negative next-operator chain [" ++
                                     show n ++ "]"
  | otherwise                        = VTSL $ (!! n) $ iterate Previous x
vPreviousN (VInt n) (VPTerm x)
  | n < 0                            = VError undefined $
                                     "Negative next-operator chain: [" ++
                                     show n ++ "]"
  | otherwise                        = VTSL $ (!! n) $ iterate Previous $ Check x
vPreviousN (VInt n) (VSTerm (Signal x))
  | n < 0                            = VError undefined $
                                     "Negative next-operator chain: [" ++
                                     show n ++ "]"
  | otherwise                        = VTSL $ (!! n) $ iterate Previous $ cb x
vPreviousN _        _                = assert False undefined

-----------------------------------------------------------------------------

vGlobally
  :: Value -> Value

vGlobally (VBoolean True)     = VBoolean True
vGlobally (VBoolean False)    = VBoolean False
vGlobally (VPTerm x)          = VTSL $ Globally $ Check x
vGlobally (VSTerm (Signal x)) = VTSL $ Globally $ cb x
vGlobally (VTSL x)            = VTSL $ Globally x
vGlobally _                   = assert False undefined

-----------------------------------------------------------------------------

vGloballyR
  :: Value -> Value -> Value -> Value

vGloballyR _        _        (VBoolean True)  = VBoolean True
vGloballyR _        _        (VBoolean False) = VBoolean False
vGloballyR (VInt n) (VInt m) (VTSL x)
  | n > m                                     = VBoolean True
  | n >= 0 && m >= 0                             =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> And [x, Next a]) x [0, 1 .. m - n - 1]
  | otherwise                                 =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"

vGloballyR (VInt n) (VInt m) (VPTerm x)
  | n > m                                     = VBoolean True
  | n >= 0 && m >= 0                             =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> And [Check x, Next a]) (Check x) [0, 1 .. m - n - 1]
  | otherwise                                 =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vGloballyR (VInt n) (VInt m) (VSTerm (Signal x))
  | n > m                                     = VBoolean True
  | n >= 0 && m >= 0                             =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> And [cb x, Next a]) (cb x) [0, 1 .. m - n - 1]
  | otherwise                                 =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vGloballyR _        _        _                = assert False undefined

-----------------------------------------------------------------------------

vFinally
  :: Value -> Value

vFinally (VBoolean True)     = VBoolean True
vFinally (VBoolean False)    = VBoolean False
vFinally (VPTerm x)          = VTSL $ Finally $ Check x
vFinally (VSTerm (Signal x)) = VTSL $ Finally $ cb x
vFinally (VTSL x)            = VTSL $ Finally x
vFinally _                   = assert False undefined

-----------------------------------------------------------------------------

vFinallyR
  :: Value -> Value -> Value -> Value

vFinallyR _        _        (VBoolean True)  = VBoolean True
vFinallyR _        _        (VBoolean False) = VBoolean False
vFinallyR (VInt n) (VInt m) (VTSL x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> Or [x, Next a]) x [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vFinallyR (VInt n) (VInt m) (VPTerm x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> Or [Check x, Next a]) (Check x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vFinallyR (VInt n) (VInt m) (VSTerm (Signal x))
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Next $
        foldl (\a _ -> Or [cb x, Next a]) (cb x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vFinallyR _        _        _                = assert False undefined

-----------------------------------------------------------------------------

vHistorically
  :: Value -> Value

vHistorically (VBoolean True)     = VBoolean True
vHistorically (VBoolean False)    = VBoolean False
vHistorically (VPTerm x)          = VTSL $ Historically $ Check x
vHistorically (VSTerm (Signal x)) = VTSL $ Historically $ cb x
vHistorically (VTSL x)            = VTSL $ Historically x
vHistorically _                   = assert False undefined

-----------------------------------------------------------------------------

vHistoricallyR
  :: Value -> Value -> Value -> Value

vHistoricallyR _        _        (VBoolean True)  = VBoolean True
vHistoricallyR _        _        (VBoolean False) = VBoolean False
vHistoricallyR (VInt n) (VInt m) (VTSL x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [x, Previous a]) x [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vHistoricallyR (VInt n) (VInt m) (VPTerm x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [Check x, Previous a]) (Check x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vHistoricallyR (VInt n) (VInt m) (VSTerm (Signal x))
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [cb x, Previous a]) (cb x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vHistoricallyR _        _        _                = assert False undefined

-----------------------------------------------------------------------------

vOnce
  :: Value -> Value

vOnce (VBoolean True)     = VBoolean True
vOnce (VBoolean False)    = VBoolean False
vOnce (VPTerm x)          = VTSL $ Once $ Check x
vOnce (VSTerm (Signal x)) = VTSL $ Once $ cb x
vOnce (VTSL x)            = VTSL $ Once x
vOnce _                   = assert False undefined

-----------------------------------------------------------------------------

vOnceR
  :: Value -> Value -> Value -> Value

vOnceR _        _        (VBoolean True)  = VBoolean True
vOnceR _        _        (VBoolean False) = VBoolean False
vOnceR (VInt n) (VInt m) (VTSL x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [x, Previous a]) x [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vOnceR (VInt n) (VInt m) (VPTerm x)
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [Check x, Previous a]) (Check x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vOnceR (VInt n) (VInt m) (VSTerm (Signal x))
  | n > m                                    = VBoolean False
  | n >= 0 && m >= 0                            =
      VTSL $ (!! n) $ iterate Previous $
        foldl (\a _ -> Or [cb x, Previous a]) (cb x) [0, 1 .. m - n - 1]
  | otherwise                                =
      VError undefined $ "Invalid range: [" ++ show n ++ ":" ++ show m ++ "]"
vOnceR _        _        _                = assert False undefined

-----------------------------------------------------------------------------

vUntil
  :: Value -> Value -> Value

vUntil _                   (VBoolean True)     = VBoolean True
vUntil _                   (VBoolean False)    = VBoolean False
vUntil (VBoolean True)     (VPTerm y)          = VTSL $ Finally $ Check y
vUntil (VBoolean True)     (VSTerm (Signal y)) = VTSL $ Finally $ cb y
vUntil (VBoolean True)     (VTSL y)            = VTSL $ Finally y
vUntil (VBoolean False)    (VPTerm y)          = VTSL $ Check y
vUntil (VBoolean False)    (VSTerm (Signal y)) = VTSL $ cb y
vUntil (VBoolean False)    (VTSL y)            = VTSL y
vUntil (VTSL x)            (VTSL y)            = VTSL $ Until x y
vUntil (VPTerm x)          (VTSL y)            = VTSL $ Until (Check x) y
vUntil (VSTerm (Signal x)) (VTSL y)            = VTSL $ Until (cb x) y
vUntil (VTSL x)            (VPTerm y)          = VTSL $ Until x $ Check y
vUntil (VPTerm x)          (VPTerm y)          = VTSL $ Until (Check x) $ Check y
vUntil (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Until (cb x) $ Check y
vUntil (VTSL x)            (VSTerm (Signal y)) = VTSL $ Until x $ cb y
vUntil (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Until (Check x) $ cb y
vUntil (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Until (cb x) $ cb y
vUntil _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vWeak
  :: Value -> Value -> Value

vWeak _                   (VBoolean True)     = VBoolean True
vWeak (VBoolean True)     _                   = VBoolean True
vWeak (VBoolean False)    (VBoolean False)    = VBoolean False
vWeak (VPTerm x)          (VBoolean False)    = VTSL $ Globally $ Check x
vWeak (VSTerm (Signal x)) (VBoolean False)    = VTSL $ Globally $ cb x
vWeak (VTSL x)            (VBoolean False)    = VTSL $ Globally x
vWeak (VBoolean False)    (VPTerm y)          = VTSL $ Check y
vWeak (VBoolean False)    (VSTerm (Signal y)) = VTSL $ cb y
vWeak (VBoolean False)    (VTSL y)            = VTSL y
vWeak (VTSL x)            (VTSL y)            = VTSL $ Weak x y
vWeak (VPTerm x)          (VTSL y)            = VTSL $ Weak (Check x) y
vWeak (VSTerm (Signal x)) (VTSL y)            = VTSL $ Weak (cb x) y
vWeak (VTSL x)            (VPTerm y)          = VTSL $ Weak x $ Check y
vWeak (VPTerm x)          (VPTerm y)          = VTSL $ Weak (Check x) $ Check y
vWeak (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Weak (cb x) $ Check y
vWeak (VTSL x)            (VSTerm (Signal y)) = VTSL $ Weak x $ cb y
vWeak (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Weak (Check x) $ cb y
vWeak (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Weak (cb x) $ cb y
vWeak _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vRelease
  :: Value -> Value -> Value

vRelease _                   (VBoolean False)    = VBoolean False
vRelease _                   (VBoolean True)     = VBoolean True
vRelease (VBoolean True)     (VPTerm y)          = VTSL $ Check y
vRelease (VBoolean True)     (VSTerm (Signal y)) = VTSL $ cb y
vRelease (VBoolean True)     (VTSL y)            = VTSL y
vRelease (VBoolean False)    (VPTerm y)          = VTSL $ Globally $ Check y
vRelease (VBoolean False)    (VSTerm (Signal y)) = VTSL $ Globally $ cb y
vRelease (VBoolean False)    (VTSL y)            = VTSL $ Globally y
vRelease (VTSL x)            (VTSL y)            = VTSL $ Release x y
vRelease (VPTerm x)          (VTSL y)            = VTSL $ Release (Check x) y
vRelease (VSTerm (Signal x)) (VTSL y)            = VTSL $ Release (cb x) y
vRelease (VTSL x)            (VPTerm y)          = VTSL $ Release x $ Check y
vRelease (VPTerm x)          (VPTerm y)          = VTSL $ Release (Check x) $ Check y
vRelease (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Release (cb x) $ Check y
vRelease (VTSL x)            (VSTerm (Signal y)) = VTSL $ Release x $ cb y
vRelease (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Release (Check x) $ cb y
vRelease (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Release (cb x) $ cb y
vRelease _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vAsSoonAs
  :: Value -> Value -> Value

vAsSoonAs (VBoolean True)     _                   = VBoolean True
vAsSoonAs _                   (VBoolean False)    = VBoolean True
vAsSoonAs (VBoolean False)    (VBoolean True)     = VBoolean False
vAsSoonAs (VBoolean False)    (VPTerm y)          = VTSL $ Globally $ Not $ Check y
vAsSoonAs (VBoolean False)    (VSTerm (Signal y)) = VTSL $ Globally $ Not $ cb y
vAsSoonAs (VBoolean False)    (VTSL y)            = VTSL $ Globally $ Not y
vAsSoonAs (VPTerm x)          (VBoolean True)     = VTSL $ Check x
vAsSoonAs (VSTerm (Signal x)) (VBoolean True)     = VTSL $ cb x
vAsSoonAs (VTSL x)            (VBoolean True)     = VTSL x
vAsSoonAs (VTSL x)            (VTSL y)            = VTSL $ Weak (Not y) $ And [x, y]
vAsSoonAs (VPTerm x)          (VTSL y)            = VTSL $ Weak (Not y) $ And [Check x, y]
vAsSoonAs (VSTerm (Signal x)) (VTSL y)            = VTSL $ Weak (Not y) $ And [cb x, y]
vAsSoonAs (VTSL x)            (VPTerm y)          = VTSL $ Weak (Not $ Check y) $ And [x, Check y]
vAsSoonAs (VPTerm x)          (VPTerm y)          = VTSL $ Weak (Not $ Check y) $ And [Check x, Check y]
vAsSoonAs (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Weak (Not $ Check y) $ And [cb x, Check y]
vAsSoonAs (VTSL x)            (VSTerm (Signal y)) = VTSL $ Weak (Not $ cb y) $ And [x, cb y]
vAsSoonAs (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Weak (Not $ cb y) $ And [Check x, cb y]
vAsSoonAs (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Weak (Not $ cb y) $ And [cb x, cb y]
vAsSoonAs _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vSince
  :: Value -> Value -> Value

vSince _                   (VBoolean True)     = VBoolean True
vSince _                   (VBoolean False)    = VBoolean False
vSince (VBoolean True)     (VPTerm y)          = VTSL $ Once $ Check y
vSince (VBoolean True)     (VSTerm (Signal y)) = VTSL $ Once $ cb y
vSince (VBoolean True)     (VTSL y)            = VTSL $ Once y
vSince (VBoolean False)    (VPTerm y)          = VTSL $ Check y
vSince (VBoolean False)    (VSTerm (Signal y)) = VTSL $ cb y
vSince (VBoolean False)    (VTSL y)            = VTSL y
vSince (VTSL x)            (VTSL y)            = VTSL $ Since x y
vSince (VPTerm x)          (VTSL y)            = VTSL $ Since (Check x) y
vSince (VSTerm (Signal x)) (VTSL y)            = VTSL $ Since (cb x) y
vSince (VTSL x)            (VPTerm y)          = VTSL $ Since x $ Check y
vSince (VPTerm x)          (VPTerm y)          = VTSL $ Since (Check x) $ Check y
vSince (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Since (cb x) $ Check y
vSince (VTSL x)            (VSTerm (Signal y)) = VTSL $ Since x $ cb y
vSince (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Since (Check x) $ cb y
vSince (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Since (cb x) $ cb y
vSince _                   _                   = assert False undefined

-----------------------------------------------------------------------------

vTriggered
  :: Value -> Value -> Value

vTriggered _                   (VBoolean False)    = VBoolean False
vTriggered _                   (VBoolean True)     = VBoolean True
vTriggered (VBoolean True)     (VPTerm y)          = VTSL $ Check y
vTriggered (VBoolean True)     (VSTerm (Signal y)) = VTSL $ cb y
vTriggered (VBoolean True)     (VTSL y)            = VTSL y
vTriggered (VBoolean False)    (VPTerm y)          = VTSL $ Historically $ Check y
vTriggered (VBoolean False)    (VSTerm (Signal y)) = VTSL $ Historically $ cb y
vTriggered (VBoolean False)    (VTSL y)            = VTSL $ Historically y
vTriggered (VTSL x)            (VTSL y)            = VTSL $ Triggered x y
vTriggered (VPTerm x)          (VTSL y)            = VTSL $ Triggered (Check x) y
vTriggered (VSTerm (Signal x)) (VTSL y)            = VTSL $ Triggered (cb x) y
vTriggered (VTSL x)            (VPTerm y)          = VTSL $ Triggered x $ Check y
vTriggered (VPTerm x)          (VPTerm y)          = VTSL $ Triggered (Check x) $ Check y
vTriggered (VSTerm (Signal x)) (VPTerm y)          = VTSL $ Triggered (cb x) $ Check y
vTriggered (VTSL x)            (VSTerm (Signal y)) = VTSL $ Triggered x $ cb y
vTriggered (VPTerm x)          (VSTerm (Signal y)) = VTSL $ Triggered (Check x) $ cb y
vTriggered (VSTerm (Signal x)) (VSTerm (Signal y)) = VTSL $ Triggered (cb x) $ cb y
vTriggered _                   _                   = assert False undefined

-----------------------------------------------------------------------------

cb
  :: a -> Formula a

cb =
  Check . BooleanInput

-----------------------------------------------------------------------------

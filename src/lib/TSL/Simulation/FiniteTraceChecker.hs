-- |
-- Module      :  TSL.Simulation.FinitTraceChecker
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple finite trace checker
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.FiniteTraceChecker
  ( FiniteTrace
  , append
  , rewind
  , emptyTrace
  , (|=)
  ) where

-----------------------------------------------------------------------------
import TSL.Logic as Logic (Formula(..), PredicateTerm, SignalTerm)

-----------------------------------------------------------------------------
-- | A Finite Trace is List of updates and predicate evalutations 
-- (which are partial functions), a finite trace can be extended by append,
-- or rewind
--
newtype FiniteTrace c =
  FiniteTrace [(c -> SignalTerm c, PredicateTerm c -> Bool)]

-----------------------------------------------------------------------------
-- | Adds an update and predicate evaluation at the end of a finite trace
append ::
     FiniteTrace c
  -> (c -> SignalTerm c)
  -> (PredicateTerm c -> Bool)
  -> FiniteTrace c
append (FiniteTrace tr) updates predicates =
  FiniteTrace $ tr ++ [(updates, predicates)]

-----------------------------------------------------------------------------
-- | Reverts the last appending to the finite trace. If the trace is empty
-- the trace stays empty
rewind :: FiniteTrace c -> FiniteTrace c
rewind (FiniteTrace ts) =
  case reverse ts of
    [] -> FiniteTrace []
    (_:tr) -> FiniteTrace (reverse tr)

-----------------------------------------------------------------------------
-- | The empty finite trace
emptyTrace :: FiniteTrace c
emptyTrace = FiniteTrace []

-----------------------------------------------------------------------------
-- | This relation is the satisfcation relation of a finite trace of a TSL formula
-- Checking is done by expansion. The checking function uses a three value logic
-- where the neutral value is used if a trace ends. Satisifcation holds when 
-- the outcoming value is not false (in the three value logic). This means
-- that the finite trace is a bad prefix the formula
(|=) :: Eq c => FiniteTrace c -> Formula c -> Bool
(|=) (FiniteTrace ts) f = check [] ts f /= FF

check ::
     Eq c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Formula c
  -> TBool
check [] [] =
  \case
    FFalse -> FF
    TTrue -> TT
    Previous _ -> FF
    _ -> NN
check (o:old) [] =
  \case
    FFalse -> FF
    TTrue -> TT
    Previous f -> check old [o] f
    Once f -> check old [o] (Once f)
    Historically f -> check old [o] (Historically f)
    Since f1 f2 -> check old [o] (Since f1 f2)
    Triggered f1 f2 -> check old [o] (Triggered f1 f2)
    _ -> NN
check old ts@(t:tr) =
  \formula ->
    case optimize formula of
      TTrue -> TT
      FFalse -> FF
    --None temporal Base
      Check p -> fromBool $ (snd t) p
      Update c st -> fromBool $ (fst t) c == st
      Not f -> tNot $ check old ts f
      And fs -> tConj $ map (check old ts) fs
      Or fs -> tDisj $ map (check old ts) fs
    --None temporal derived
      Implies f1 f2 -> check old ts $ Or [Not f1, f2]
      Equiv f1 f2 -> check old ts $ And [Implies f1 f2, Implies f2 f1]
    -- Temporal, note that H, T cant be expanded due to the missing Z operator
      Next f -> check (t : old) tr f
      Previous f ->
        case old of
          [] -> FF
          (o:ol) -> check ol (o : ts) f
      Historically f ->
        (check old ts f) &&&
        case old of
          [] -> TT
          (o:ol) -> check ol (o : ts) (Historically f)
      Triggered f1 f2 ->
        (check old ts f2) &&&
        ((check old ts f1) |||
         case old of
           [] -> TT
           (o:ol) -> check ol (o : ts) (Triggered f1 f2))
    -- Temporal Expanded
      Globally f -> check old ts $ And [f, Next (Globally f)]
      Finally f -> check old ts $ Or [f, Next (Finally f)]
      Until f1 f2 -> check old ts $ Or [f2, And [f1, Next (Until f1 f2)]]
      Weak f1 f2 -> check old ts $ Or [f2, And [f1, Next (Until f1 f2)]]
      Release f1 f2 -> check old ts $ Weak f2 (And [f1, f2])
      Once f -> check old ts $ Or [f, Previous (Once f)]
      Since f1 f2 -> check old ts $ Or [f2, And [f1, Previous (Since f1 f2)]]

-----------------------------------------------------------------------------
-- | Expansion and Lifting based optimizations 
optimize :: Formula a -> Formula a
optimize = liftNext . expand

liftNext :: Formula a -> Formula a
liftNext =
  \case
    Not f ->
      case liftNext f of
        Next f' -> Next (Not f')
        f' -> Not f'
    And fs ->
      case liftNextList $ map liftNext fs of
        ([], []) -> TTrue
        (gs, []) -> And gs
        ([], hs) -> Next $ And hs
        (gs, hs) -> And $ gs ++ [Next $ And hs]
    Or fs ->
      case liftNextList $ map liftNext fs of
        ([], []) -> TTrue
        (gs, []) -> Or gs
        ([], hs) -> Next $ Or hs
        (gs, hs) -> Or $ gs ++ [Next $ Or hs]
    f -> f
  where
    liftNextList :: [Formula a] -> ([Formula a], [Formula a])
    liftNextList [] = ([], [])
    liftNextList (Next f:xr) =
      let (a, b) = liftNextList xr
       in (a, f : b)
    liftNextList (f:xr) =
      let (a, b) = liftNextList xr
       in (f : a, b)

expand :: Formula a -> Formula a
expand =
  \case
    TTrue -> TTrue
    FFalse -> FFalse
    Check p -> Check p
    Update c st -> Update c st
    Not f ->
      case expand f of
        TTrue -> FFalse
        FFalse -> TTrue
        And fs -> Or $ map (expand . Not) fs
        Or fs -> And $ map (expand . Not) fs
        Next f -> Next (Not f)
        f -> Not f
    And fs -> And (map expand fs)
    Or fs -> Or (map expand fs)
    Implies f1 f2 -> expand $ Or [Not f1, f2]
    Equiv f1 f2 -> expand $ And [Implies f1 f2, Implies f2 f1]
    Next f -> Next f
    Previous f -> Previous f
    Historically f -> Historically f
    Triggered f1 f2 -> Triggered f1 f2
    Globally f -> expand $ And [f, Next (Globally f)]
    Finally f -> expand $ Or [f, Next (Finally f)]
    Until f1 f2 -> expand $ Or [f2, And [f1, Next (Until f1 f2)]]
    Weak f1 f2 -> expand $ Or [f2, And [f1, Next (Until f1 f2)]]
    Release f1 f2 -> expand $ Weak f2 (And [f1, f2])
    Once f -> expand $ Or [f, Previous (Once f)]
    Since f1 f2 -> expand $ Or [f2, And [f1, Previous (Since f1 f2)]]

-----------------------------------------------------------------------------
-- | Data structure for a three value logic
data TBool
  = TT
  | NN
  | FF
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------
-- | Converts a bool to a three value bool
fromBool :: Bool -> TBool
fromBool True = TT
fromBool False = FF

-----------------------------------------------------------------------------
-- | Three value disjunction
(|||) :: TBool -> TBool -> TBool
(|||) TT _ = TT
(|||) _ TT = TT
(|||) NN _ = NN
(|||) _ NN = NN
(|||) _ _ = FF

-----------------------------------------------------------------------------
-- | Three value conjunction
(&&&) :: TBool -> TBool -> TBool
(&&&) FF _ = FF
(&&&) _ FF = FF
(&&&) NN _ = NN
(&&&) _ NN = NN
(&&&) _ _ = TT

-----------------------------------------------------------------------------
-- | Three value negation
tNot :: TBool -> TBool
tNot TT = FF
tNot NN = NN
tNot FF = NN

-----------------------------------------------------------------------------
-- | Three value list conjunction (conjunct all elements of the list)
tConj :: [TBool] -> TBool
tConj [] = TT
tConj (x:xr) = x &&& tConj xr

-----------------------------------------------------------------------------
-- | Three value list disjunction (conjunct all elements of the list)
tDisj :: [TBool] -> TBool
tDisj [] = FF
tDisj (x:xr) = x ||| tDisj xr

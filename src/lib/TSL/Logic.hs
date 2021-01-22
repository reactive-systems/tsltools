-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Logic
-- Maintainer  :  Felix Klein
--
-- TSL logic data type and utility functions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL.Logic
  ( Formula(..)
  , SignalTerm(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , updates
  , checks
  , inputs
  , outputs
  , functions
  , predicates
  , symbols
  , tslFormula
  , tlsfFormula
  , encodeInputAP
  , encodeOutputAP
  , decodeInputAP
  , decodeOutputAP
  , exactlyOne
  , size
  ) where

-----------------------------------------------------------------------------

import Data.Char (isUpper, toLower, toUpper)

import Data.Set (Set, difference, empty, insert, unions)

import qualified Data.Set as S (map)

import Control.Monad (void)

import TSL.Error (Error, parseError)

import Text.Parsec (alphaNum, char, eof, lookAhead, parse, string, try, (<|>))

import Text.Parsec.String (Parser)

import Test.QuickCheck (Arbitrary, arbitrary, choose)

-----------------------------------------------------------------------------

-- | Representation of TSL signal terms.

data SignalTerm a =
    Signal a
  | FunctionTerm (FunctionTerm a)
  | PredicateTerm (PredicateTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor SignalTerm where
  fmap f = \case
    Signal s        -> Signal $ f s
    FunctionTerm t  -> FunctionTerm $ fmap f t
    PredicateTerm t -> PredicateTerm $ fmap f t

instance Foldable SignalTerm where
  foldr f a = \case
    Signal s        -> f s a
    FunctionTerm t  -> foldr f a t
    PredicateTerm t -> foldr f a t

instance Arbitrary a => Arbitrary (SignalTerm a) where
  arbitrary =
    choose (0 :: Int, 2 :: Int) >>= \case
      0 -> Signal <$> arbitrary
      1 -> FunctionTerm <$> arbitrary
      _ -> PredicateTerm <$> arbitrary

-----------------------------------------------------------------------------

-- | Representation of TSL function terms.

data FunctionTerm a =
    FunctionSymbol a
  | FApplied (FunctionTerm a) (SignalTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor FunctionTerm where
  fmap f = \case
    FunctionSymbol s -> FunctionSymbol $ f s
    FApplied t t'    -> FApplied (fmap f t) (fmap f t')

instance Foldable FunctionTerm where
  foldr f a = \case
    FunctionSymbol s -> f s a
    FApplied t t'    -> foldr f (foldr f a t) t'

instance Arbitrary a => Arbitrary (FunctionTerm a) where
  arbitrary = do
    b <- arbitrary
    if b
    then FunctionSymbol <$> arbitrary
    else FApplied <$> arbitrary <*> arbitrary

-----------------------------------------------------------------------------

-- | Represenation of TSL predicate terms.

data PredicateTerm a =
    BooleanTrue
  | BooleanFalse
  | BooleanInput a
  | PredicateSymbol a
  | PApplied (PredicateTerm a) (SignalTerm a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor PredicateTerm where
  fmap f = \case
    BooleanTrue       -> BooleanTrue
    BooleanFalse      -> BooleanFalse
    BooleanInput s    -> BooleanInput $ f s
    PredicateSymbol s -> PredicateSymbol $ f s
    PApplied t t'     -> PApplied (fmap f t) (fmap f t')

instance Foldable PredicateTerm where
  foldr f a = \case
    BooleanTrue       -> a
    BooleanFalse      -> a
    BooleanInput s    -> f s a
    PredicateSymbol s -> f s a
    PApplied t t'     -> foldr f (foldr f a t) t'

instance Arbitrary a => Arbitrary (PredicateTerm a) where
  arbitrary =
    choose (0 :: Int, 4 :: Int) >>= \case
      0 -> return BooleanTrue
      1 -> return BooleanFalse
      2 -> BooleanInput <$> arbitrary
      3 -> PredicateSymbol <$> arbitrary
      _ -> PApplied <$> arbitrary <*> arbitrary

-----------------------------------------------------------------------------

data Formula a =
    TTrue
  | FFalse
  | Check (PredicateTerm a)
  | Update a (SignalTerm a)
  | Not (Formula a)
  | Implies (Formula a) (Formula a)
  | Equiv (Formula a) (Formula a)
  | And [Formula a]
  | Or [Formula a]
  | Next (Formula a)
  | Previous (Formula a)
  | Globally (Formula a)
  | Finally (Formula a)
  | Historically (Formula a)
  | Once (Formula a)
  | Until (Formula a) (Formula a)
  | Release (Formula a) (Formula a)
  | Weak (Formula a) (Formula a)
  | Since (Formula a) (Formula a)
  | Triggered (Formula a) (Formula a)
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

instance Functor Formula where
  fmap f = \case
    TTrue          -> TTrue
    FFalse         -> FFalse
    Check t        -> Check $ fmap f t
    Update s t     -> Update (f s) $ fmap f t
    Not x          -> Not $ fmap f x
    Implies x y    -> Implies (fmap f x) $ fmap f y
    Equiv x y      -> Equiv (fmap f x) $ fmap f y
    And xs         -> And $ fmap (fmap f) xs
    Or xs          -> Or $ fmap (fmap f) xs
    Next x         -> Next $ fmap f x
    Previous x     -> Previous $ fmap f x
    Globally x     -> Globally $ fmap f x
    Finally x      -> Finally $ fmap f x
    Historically x -> Historically $ fmap f x
    Once x         -> Once $ fmap f x
    Until x y      -> Until (fmap f x) $ fmap f y
    Release x y    -> Release (fmap f x) $ fmap f y
    Weak x y       -> Weak (fmap f x) $ fmap f y
    Since x y      -> Since (fmap f x) $ fmap f y
    Triggered x y  -> Triggered (fmap f x) $ fmap f y

instance Foldable Formula where
  foldr f a = \case
    TTrue          -> a
    FFalse         -> a
    Check t        -> foldr f a t
    Update s t     -> foldr f (f s a) t
    Not x          -> foldr f a x
    Implies x y    -> foldr f (foldr f a x) y
    Equiv x y      -> foldr f (foldr f a x) y
    And xs         -> foldr (flip $ foldr f) a xs
    Or xs          -> foldr (flip $ foldr f) a xs
    Next x         -> foldr f a x
    Previous x     -> foldr f a x
    Globally x     -> foldr f a x
    Finally x      -> foldr f a x
    Historically x -> foldr f a x
    Once x         -> foldr f a x
    Until x y      -> foldr f (foldr f a x) y
    Release x y    -> foldr f (foldr f a x) y
    Weak x y       -> foldr f (foldr f a x) y
    Since x y      -> foldr f (foldr f a x) y
    Triggered x y  -> foldr f (foldr f a x) y

-----------------------------------------------------------------------------

-- | Returns the size of the given TSL formula.

size
  :: Formula a -> Int

size = size' 0

  where
    size' a = \case
      TTrue          -> a + 1
      FFalse         -> a + 1
      Check {}       -> a + 1
      Update {}      -> a + 1
      Not x          -> size' (a + 1) x
      Implies x y    -> size' (size' (a + 1) x) y
      Equiv x y      -> size' (size' (a + 1) x) y
      And xs         -> foldl size' (a + 1) xs
      Or xs          -> foldl size' (a + 1) xs
      Next x         -> size' (a + 1) x
      Previous x     -> size' (a + 1) x
      Globally x     -> size' (a + 1) x
      Finally x      -> size' (a + 1) x
      Historically x -> size' (a + 1) x
      Once x         -> size' (a + 1) x
      Until x y      -> size' (size' (a + 1) x) y
      Release x y    -> size' (size' (a + 1) x) y
      Weak x y       -> size' (size' (a + 1) x) y
      Since x y      -> size' (size' (a + 1) x) y
      Triggered x y  -> size' (size' (a + 1) x) y

-----------------------------------------------------------------------------

-- | Returns all predicate terms that are checked as part of the formula.

checks
  :: Ord a => Formula a -> Set (PredicateTerm a)

checks = preds empty
  where
    preds s = \case
      TTrue          -> s
      FFalse         -> s
      Update {}      -> s
      Check p        -> insert p s
      Not x          -> preds s x
      Implies x y    -> preds (preds s x) y
      Equiv x y      -> preds (preds s x) y
      And xs         -> foldl preds s xs
      Or xs          -> foldl preds s xs
      Next x         -> preds s x
      Previous x     -> preds s x
      Globally x     -> preds s x
      Finally x      -> preds s x
      Historically x -> preds s x
      Once x         -> preds s x
      Until x y      -> preds (preds s x) y
      Release x y    -> preds (preds s x) y
      Weak x y       -> preds (preds s x) y
      Since x y      -> preds (preds s x) y
      Triggered x y  -> preds (preds s x) y

-----------------------------------------------------------------------------

-- | Returns all updates that appear in the formula

updates
  :: Ord a => Formula a -> Set (a, SignalTerm a)

updates = upds empty
  where
    upds s = \case
      TTrue          -> s
      FFalse         -> s
      Check {}       -> s
      Update x t     -> insert (x,t) s
      Not x          -> upds s x
      Implies x y    -> upds (upds s x) y
      Equiv x y      -> upds (upds s x) y
      And xs         -> foldl upds s xs
      Or xs          -> foldl upds s xs
      Next x         -> upds s x
      Previous x     -> upds s x
      Globally x     -> upds s x
      Finally x      -> upds s x
      Historically x -> upds s x
      Once x         -> upds s x
      Until x y      -> upds (upds s x) y
      Release x y    -> upds (upds s x) y
      Weak x y       -> upds (upds s x) y
      Since x y      -> upds (upds s x) y
      Triggered x y  -> upds (upds s x) y

-----------------------------------------------------------------------------

outputs
  :: Ord a => Formula a -> Set a

outputs = S.map fst . updates

-----------------------------------------------------------------------------

inputs
  :: Ord a => Formula a -> Set a

inputs fml =
  let
    ps = checks fml
    os = outputs fml
    fs = S.map snd $ updates fml
  in
    difference (foldl sti (foldl pti empty ps) fs) os

  where
    pti a = \case
      PApplied p s   -> sti (pti a p) s
      BooleanInput x -> insert x a
      _              -> a

    fti a = \case
      FApplied f s -> sti (fti a f) s
      _            -> a

    sti a = \case
      PredicateTerm p -> pti a p
      FunctionTerm f  -> fti a f
      Signal x        -> insert x a

-----------------------------------------------------------------------------

functions
  :: Ord a => Formula a -> Set a

functions fml =
  let
    ps = checks fml
    fs = S.map snd $ updates fml
  in
    foldl sti (foldl pti empty ps) fs

  where
    pti a = \case
      PApplied p s      -> sti (pti a p) s
      PredicateSymbol x -> insert x a
      _                 -> a

    fti a = \case
      FApplied f s     -> sti (fti a f) s
      FunctionSymbol x -> insert x a

    sti a = \case
      PredicateTerm p -> pti a p
      FunctionTerm f  -> fti a f
      _               -> a

-----------------------------------------------------------------------------

predicates
  :: Ord a => Formula a -> Set a

predicates fml =
  let
    ps = checks fml
    fs = S.map snd $ updates fml
  in
    foldl sti (foldl pti empty ps) fs

  where
    pti a = \case
      PApplied p s      -> sti (pti a p) s
      PredicateSymbol x -> insert x a
      _                 -> a

    fti a = \case
      FApplied f s -> sti (fti a f) s
      _            -> a

    sti a = \case
      PredicateTerm p -> pti a p
      FunctionTerm f  -> fti a f
      _               -> a

-----------------------------------------------------------------------------

symbols
  :: Ord a => Formula a -> Set a
symbols = unions . ((<*>) [inputs, outputs, functions, predicates]) . pure

-----------------------------------------------------------------------------

exactlyOne
  :: [Formula a ] -> Formula a

exactlyOne xs =
  case split ([],[]) xs of
    ( [], []) -> FFalse
    ( [],[x]) -> x
    ([x], []) -> x
    ( ys, zs) ->
      Not $ Equiv
        (And [exactlyOne ys, Not $ Or zs])
        (And [exactlyOne zs, Not $ Or ys])

  where
    split (a,b) = \case
      (x:xr) -> split (b,x:a) xr
      []     -> (a,b)

-----------------------------------------------------------------------------

-- | Converts a formula to TSL.

tslFormula
  :: (a -> String) -> Formula a -> String

tslFormula sig = prFml
  where
    prFml = \case
      TTrue          -> prT
      FFalse         -> prF
      Check p        -> prPT p
      Update x s     -> "[" ++ sig x ++ " <- " ++ prST s ++ "]"
      Not x          -> br $ "! " ++ prFml x
      And xs         -> br $ concat $ insertInside " && " $ map prFml xs
      Or xs          -> br $ concat $ insertInside " || " $ map prFml xs
      Implies x y    -> br $ prFml x ++ " -> " ++ prFml y
      Equiv x y      -> br $ prFml x ++ " <-> " ++ prFml y
      Next x         -> br $ "X " ++ prFml x
      Previous x     -> br $ "Y " ++ prFml x
      Globally x     -> br $ "G " ++ prFml x
      Finally x      -> br $ "F " ++ prFml x
      Historically x -> br $ "H " ++ prFml x
      Once x         -> br $ "O " ++ prFml x
      Until x y      -> br $ prFml x ++ " U " ++ prFml y
      Release x y    -> br $ prFml x ++ " R " ++ prFml y
      Weak x y       -> br $ prFml x ++ " W " ++ prFml y
      Since x y      -> br $ prFml x ++ " S " ++ prFml y
      Triggered x y  -> br $ prFml x ++ " T " ++ prFml y

    br = ('(':) . (++ ")")

    insertInside a = \case
      []   -> []
      [x]  -> [x]
      x:xr -> x : a : insertInside a xr

    prT = "true"

    prF = "false"

    prPT = \case
      BooleanTrue       -> prT
      BooleanFalse      -> prF
      BooleanInput x    -> sig x
      PredicateSymbol x -> sig x
      PApplied p s      -> br $ prPT p ++ " " ++ prST s

    prST = \case
      Signal x        -> sig x
      PredicateTerm p -> prPT p
      FunctionTerm f  -> case f of
        FunctionSymbol x -> sig x ++ "()"
        _                -> prFT f

    prFT = \case
      FunctionSymbol x -> sig x
      FApplied f s     -> br $ prFT f ++ " " ++ prST s

-----------------------------------------------------------------------------

-- | Converts a formula to TLSF.

tlsfFormula
  :: (a -> String) -> Formula a -> String

tlsfFormula f = pr
  where
    pr = \case
      TTrue          -> "true"
      FFalse         -> "false"
      Check t        -> encodeInputAP f t
      Update s t     -> encodeOutputAP f s t
      Not x          -> "! (" ++ pr x ++ ")"
      Implies x y    -> "(" ++ pr x ++ ") -> (" ++ pr y ++ ")"
      Equiv x y      -> "(" ++ pr x ++ ") <-> (" ++ pr y ++ ")"
      And []         -> "true"
      And [x]        -> pr x
      And (x:xr)     -> foldl (\a y -> "(" ++ a ++ ") && (" ++ pr y ++ ")") (pr x) xr
      Or []          -> "false"
      Or [x]         -> pr x
      Or (x:xr)      -> foldl (\a y -> "(" ++ a ++ ") || (" ++ pr y ++ ")") (pr x) xr
      Next x         -> "X (" ++ pr x ++ ")"
      Previous x     -> "Y (" ++ pr x ++ ")"
      Globally x     -> "G (" ++ pr x ++ ")"
      Finally x      -> "F (" ++ pr x ++ ")"
      Historically x -> "H (" ++ pr x ++ ")"
      Once x         -> "O (" ++ pr x ++ ")"
      Until x y      -> "(" ++ pr x ++ ") U (" ++ pr y ++ ")"
      Release x y    -> "(" ++ pr x ++ ") R (" ++ pr y ++ ")"
      Weak x y       -> "(" ++ pr x ++ ") W (" ++ pr y ++ ")"
      Since x y      -> "(" ++ pr x ++ ") S (" ++ pr y ++ ")"
      Triggered x y  -> "(" ++ pr x ++ ") T (" ++ pr y ++ ")"

-----------------------------------------------------------------------------

encodeInputAP
  :: (a -> String) -> PredicateTerm a -> String

encodeInputAP f =
  ("p0" ++) . encodePredicate f

-----------------------------------------------------------------------------

encodeOutputAP
  :: (a -> String) -> a -> SignalTerm a -> String

encodeOutputAP f s t =
  "u0" ++ escape (f s) ++ "0" ++ encodeSignal f t

-----------------------------------------------------------------------------

-- | Converts a function term to a TLSF identifier.

encodeFunction
  :: (a -> String) -> FunctionTerm a -> String

encodeFunction f = \case
  FunctionSymbol s -> escape (f s)
  FApplied t t'    -> encodeFunction f t ++ "0" ++ encodeSignal f t'

-----------------------------------------------------------------------------

-- | Converts a predicate term to a TLSF identifier.

encodePredicate
  :: (a -> String) -> PredicateTerm a -> String

encodePredicate f = \case
  BooleanTrue       -> "bt"
  BooleanFalse      -> "bf"
  BooleanInput s    -> "b0" ++ escape (f s)
  PredicateSymbol s -> "p0" ++ escape (f s)
  PApplied t t'     -> encodePredicate f t ++ "0" ++ encodeSignal f t'

-----------------------------------------------------------------------------

-- | Conversts a signal term to a TLSF identifier.

encodeSignal
  :: (a -> String) -> SignalTerm a -> String

encodeSignal f = \case
  Signal s        -> escape (f s)
  FunctionTerm t  -> "f1d" ++ encodeFunction f t ++ "1b"
  PredicateTerm t -> "p1d" ++ encodePredicate f t ++ "1b"

-----------------------------------------------------------------------------

-- | Parses the term structure from a generated TSLF input.

decodeInputAP
  :: String -> Either Error (PredicateTerm String)

decodeInputAP str =
  case parse (string "p0" >> predicateParser) "Format Error" str of
    Left err -> parseError err
    Right x  -> return x

-----------------------------------------------------------------------------

-- | Parses the term structure from a generated TSLF output.

decodeOutputAP
  :: String -> Either Error (String, SignalTerm String)

decodeOutputAP str =
  case parse outputParser "Format Error" str of
    Left err -> parseError err
    Right x  -> return x

  where
    outputParser = do
      void $ string "u0"
      u <- identParser
      void $ char '0'
      s <- parseSignal
      return (u, s)

    parseSignal =
          (try (string "p1d") >> (PredicateTerm <$> predicateParser))
      <|> (try (string "f1d") >> (FunctionTerm <$> functionParser))
      <|> (Signal <$> identParser)

-----------------------------------------------------------------------------

functionParser
  :: Parser (FunctionTerm String)

functionParser = do
  f <- identParser
  next $ FunctionSymbol f

  where
    next f =
          (char '0'    >> parseArgs >>= next . FApplied f)
      <|> (string "1b" >> return f)
      <|> (eof         >> return f)

    parseArgs =
          (try (string "p1d") >> (PredicateTerm <$> predicateParser))
      <|> (try (string "f1d") >> (FunctionTerm <$> functionParser))
      <|> (Signal <$> identParser)

-----------------------------------------------------------------------------

predicateParser
  :: Parser (PredicateTerm String)

predicateParser =
  identParser' >>= next

  where
    next p =
          (char '0'    >> parseArgs >>= next . PApplied p)
      <|> (string "1b" >> return p)
      <|> (eof         >> return p)

    parseArgs =
          (try (string "p1d") >> (PredicateTerm <$> predicateParser))
      <|> (try (string "f1d") >> (FunctionTerm <$> functionParser))
      <|> (Signal <$> identParser)

    identParser' =
      ( char 'b' >>
        (    (char 't' >> return BooleanTrue)
         <|> (char 'f' >> return BooleanFalse)
         <|> (char '0' >> BooleanInput <$> identParser)
        )
      ) <|> (string "p0" >> PredicateSymbol <$> identParser)

-----------------------------------------------------------------------------

-- | Identifier parser that reverses character escaping

identParser
  :: Parser String

identParser = ident ""

  where
    ident
      :: String -> Parser String

    ident a =
      (eof >> return (reverse a)) <|> ident1 a


    ident1
      :: String -> Parser String

    ident1 a =
      lookAhead alphaNum >>= \case
        '0' -> return $ reverse a
        '1' -> return $ reverse a
        '2' -> alphaNum >> alphaNum >>= \case
          '3' -> ident ('0' : a)
          '4' -> ident ('1' : a)
          '5' -> ident ('2' : a)
          '6' -> ident ('_' : a)
          '7' -> ident ('@' : a)
          '8' -> ident ('\'' : a)
          '9' -> ident ('.' : a)
          c   -> ident (toUpper c : a)
        c   -> alphaNum >> ident (c:a)

-----------------------------------------------------------------------------

-- | Escapes characters which potentially can cause problems.

escape
  :: String -> String

escape = escape' []
  where
    escape' a = \case
      []   -> reverse a
      c:cr -> case c of
        '0'  -> escape' ('3' : '2' : a) cr
        '1'  -> escape' ('4' : '2' : a) cr
        '2'  -> escape' ('5' : '2' : a) cr
        '_'  -> escape' ('6' : '2' : a) cr
        '@'  -> escape' ('7' : '2' : a) cr
        '\'' -> escape' ('8' : '2' : a) cr
        '.'  -> escape' ('9' : '2' : a) cr
        _
          | isUpper c -> escape' (toLower c : '2' : a) cr
          | otherwise -> escape' (c : a) cr

-----------------------------------------------------------------------------

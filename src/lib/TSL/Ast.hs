-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Ast
-- Description :  Abstract Syntax Tree form for signals, functions, and predicates.
-- Maintainer  :  Wonhyuk Choi
-- Alternate data structure format for ADT's in TSL.Logic.
-- All signal/function/predicate Abstract Data Types in TSL.Logic are curried,
-- but the AST structure can be necessary, e.g. as input to SMT/SyGuS solvers.
-- This module defines the AST type, and the transformative functions thereof.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.Ast( Ast(..)
              , AstInfo(..)
              , SymbolInfo(..)
              , (+++)
              , deduplicate
              , fromSignalTerm
              , fromFunctionTerm
              , fromPredicateTerm
              , getAstInfo
              , stringifyAst
              , astByDepth
              , getSignals
              ) where

-------------------------------------------------------------------------------

import Data.List(nub)

import Data.Map(Map)

import Control.Applicative (liftA2)

import Control.Exception (assert)

import TSL.Logic ( SignalTerm(..)
                 , FunctionTerm(..)
                 , PredicateTerm(..)
                 )

-------------------------------------------------------------------------------

-- | The Abstract Syntax Tree (AST) datatype.
-- SignalTerm as defined by Logic.hs are all curried,
-- consistent with Haskell methodology.
-- However, an AST representation can come in handy;
-- e.g. the SMT2 standard accepts specifications in AST syntax.
--
-- Example:
-- Consider the following TSL signalterm:
-- f x (g y) (h z)
-- As a Logic.SignalTerm datatype, this is structured as
-- FunctionTerm
-- (FApplied
--     (  FApplied
--        (FApplied (FunctionSymbol f) (Signal x))
--        (FunctionTerm (FApplied (FunctionSymbol g) (Signal y)))
--     )
--     (  FunctionTerm
--        (FApplied (FunctionSymbol h) (Signal z))
--     )
-- )
-- 
-- As an AST, the same signal term is 
-- Function f [ Variable x
--            , Function g [Variable y]
--            , Function h [Variable z]
--            ]
-- Which is a more compact structure.
--
-- AST's are generated from Logic.SignalTerm by using `fromSignalTerm`.
-- In order to convert the curried SignalTerm to an AST, however,
-- you need to provide a function that returns the arity of each symbol.
data Ast a = 
      Variable  a
    | Function  a [Ast a]
    | Predicate a [Ast a]

instance Show a => Show (Ast a) where
  show = \case
    (Variable  a     ) -> show a
    (Function  a args) -> "(" ++ show a ++ " " ++ (unwords (map show args)) ++ ")"
    (Predicate a args) -> "(" ++ show a ++ " " ++ (unwords (map show args)) ++ ")"

instance Functor Ast where
  fmap f = \case
    (Variable  a     ) -> Variable  (f a)
    (Function  a args) -> Function  (f a) (map (fmap f) args)
    (Predicate a args) -> Predicate (f a) (map (fmap f) args)

instance Foldable Ast where
  foldr f acc  = \case
    Variable  a      -> f a acc
    Function  a args -> f a (foldr onList acc args)
     where onList ast acc = foldr f acc ast
    Predicate a args -> foldr f acc $ Function a args

instance Applicative Ast where
  pure  = Variable
  (<*>) = liftA2 id

instance Traversable Ast where
  traverse f = \case
    Variable  a       -> Variable  <$> f a
    Function  a args  -> Function  <$> f a <*> traverse (traverse f) args
    Predicate a args  -> Predicate <$> f a <*> traverse (traverse f) args

data Annotated a = 
    VarSymbol  a
  | FuncSymbol a
  | PredSymbol a

unAnnotate :: Annotated a -> a
unAnnotate = \case
  VarSymbol  a -> a
  FuncSymbol a -> a
  PredSymbol a -> a

fromSignalTerm :: (a -> Int) -> SignalTerm a -> Ast a
fromSignalTerm arity = (fromList arity) . flattenS

fromFunctionTerm :: (a -> Int) -> FunctionTerm a -> Ast a
fromFunctionTerm arity = (fromList arity) . flattenF

fromPredicateTerm :: (a -> Int) -> PredicateTerm a -> Ast a
fromPredicateTerm arity = (fromList arity) . flattenP

flattenS :: SignalTerm a -> [Annotated a]
flattenS = \case
  Signal a -> [VarSymbol a]
  FunctionTerm f  -> flattenF f
  PredicateTerm p -> flattenP p

flattenF :: FunctionTerm a -> [Annotated a]
flattenF = \case
  FunctionSymbol a -> [FuncSymbol a]
  FApplied fterm s -> flattenF fterm ++ flattenS s

flattenP :: PredicateTerm a -> [Annotated a]
flattenP = \case
  BooleanTrue       -> error "\"True\"  cannot be part of an AST."
  BooleanFalse      -> error "\"False\" cannot be part of an AST."
  BooleanInput a    -> [VarSymbol a]
  PredicateSymbol a -> [PredSymbol a]
  PApplied pterm s  -> flattenP pterm ++ flattenS s

buildAst :: [Ast a] -> Annotated a -> Ast a
buildAst args = \case
  VarSymbol  a -> case args of
                   [] -> Variable a
                   _  -> error "Variable construction has arguments!"
  FuncSymbol a -> Function  a args
  PredSymbol a -> Predicate a args

fromList :: (a -> Int) -> [Annotated a] -> Ast a
fromList _ [] = error "Cannot construct AST from nothing"
fromList arity (x:xs) = assert usedAllArgs $ buildAst args x 
  where (args, rem) = argBuilder arity (arity' x) xs
        arity'      = arity . unAnnotate
        usedAllArgs = null rem

argBuilder :: (a -> Int) -> Int -> [Annotated a] -> ([Ast a], [Annotated a])
argBuilder _ 0 xs          = ([], xs)
argBuilder _ _ []          = error "Ast building ran out of arguments!"
argBuilder arity n (x:xs)  = (args, remainder)
  where 
    args     = argsHead:argsTail
    argsHead = buildAst headArgs x
    (argsTail, remainder ) = argBuilder arity (n-1) remainder'
    (headArgs, remainder') = argBuilder arity (arity' x) xs
    arity'                 = arity . unAnnotate

data SymbolInfo a = SymbolInfo {symbol :: a, arity :: Int}

instance Functor SymbolInfo where
  fmap f (SymbolInfo symbol arity) = SymbolInfo (f symbol) arity

instance Ord a => Ord (SymbolInfo a) where
  compare s1 s2 = compare (symbol s1) (symbol s2)

instance Eq a => Eq (SymbolInfo a) where
    t1 == t2 = (symbol t1) == (symbol t2) && (arity t1) == (arity t2)

data AstInfo a =
  AstInfo
    { varInfos  :: [SymbolInfo a]
    , funcInfos :: [SymbolInfo a]
    , predInfos :: [SymbolInfo a]
    }

infixr 5  +++
(+++) :: AstInfo a -> AstInfo a -> AstInfo a
(+++) (AstInfo v f p) (AstInfo v' f' p') = AstInfo (v ++ v') (f ++ f') (p ++ p')

deduplicate :: (Eq a) => AstInfo a -> AstInfo a
deduplicate (AstInfo v f p) = AstInfo (nub v) (nub f) (nub p)

instance Functor AstInfo where
  fmap f (AstInfo vars funcs preds) = AstInfo vars' funcs' preds'
      where vars'  = map (fmap f) vars
            funcs' = map (fmap f) funcs
            preds' = map (fmap f) preds

symbolInfosFromList
    :: (Ast a -> [SymbolInfo a])
    -> [Ast a]
    -> [SymbolInfo a]
symbolInfosFromList infoGetter = (foldr1 (++)) . (map infoGetter)

-- TODO: Better time complexity
getVarInfos :: Ast a -> [SymbolInfo a]
getVarInfos = \case
  Variable  a      -> [SymbolInfo a 0]
  Function  _ args -> symbolInfosFromList getVarInfos args
  Predicate _ args -> symbolInfosFromList getVarInfos args

-- TODO: Better time complexity
getFuncInfos :: Ast a -> [SymbolInfo a]
getFuncInfos = \case
  Variable  _      -> []
  Function  f args -> (SymbolInfo f (length args)):(symbolInfosFromList getFuncInfos args)
  Predicate _ args -> symbolInfosFromList getFuncInfos args

-- TODO: Better time complexity
getPredInfos :: Ast a -> [SymbolInfo a]
getPredInfos = \case
  Variable  _      -> []
  Function  _ args -> symbolInfosFromList getPredInfos args
  Predicate p args -> (SymbolInfo p (length args)):(symbolInfosFromList getPredInfos args)

getAstInfo :: Ast a -> AstInfo a
getAstInfo ast = AstInfo vars funcs preds
  where vars   = getVarInfos  ast
        funcs  = getFuncInfos ast
        preds  = getPredInfos ast

-- (a -> [a] -> [a]) -> [a] -> Ast a -> [a]
getSignals :: Ast a -> [a]
getSignals = \case
  Variable  a      -> [a]
  Function  _ args -> concat (map getSignals args)
  Predicate _ args -> concat (map getSignals args)

-- TODO
-- NOT CORRECT
astByDepth :: Ast a -> [Ast a]
astByDepth = \case
  v@(Variable  _     ) -> [v]
  f@(Function  _ args) -> f:(concat $ map astByDepth args)
  p@(Predicate _ args) -> p:(concat $ map astByDepth args)

levelOrderTraversal :: Int -> Ast a -> Map Int (Ast a) -> Map Int (Ast a)
levelOrderTraversal = undefined

stringifyAst :: (a -> String) -> Ast a -> String
stringifyAst stringify = \case
  Variable  v      -> stringify v
  Function  f args ->
    "(" ++ stringify f ++ " " ++ unwords (map (stringifyAst stringify) args) ++ ")"
  Predicate p args ->
    "(" ++ stringify p ++ " " ++ unwords (map (stringifyAst stringify) args) ++ ")"

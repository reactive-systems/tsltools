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
              , fromSignalTerm
              , fromFunctionTerm
              , fromPredicateTerm
              , getVars
              , stringifyAst
              ) where

-------------------------------------------------------------------------------

import Control.Applicative(liftA2)

import TSL.Logic ( SignalTerm(..)
                 , FunctionTerm(..)
                 , PredicateTerm(..)
                 )

-------------------------------------------------------------------------------

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

-- FIXME: edit with Error & Assert
flattenP :: PredicateTerm a -> [Annotated a]
flattenP = \case
  BooleanTrue       -> undefined -- Not Implemented
  BooleanFalse      -> undefined -- Not Implemented
  BooleanInput a    -> [VarSymbol a]
  PredicateSymbol a -> [PredSymbol a]
  PApplied pterm s  -> flattenP pterm ++ flattenS s

buildAst :: [Ast a] -> Annotated a -> Ast a
buildAst args = \case
  VarSymbol  a -> case args of
                   [] -> Variable a
                   _  -> undefined
  FuncSymbol a -> Function  a args
  PredSymbol a -> Predicate a args

-- FIXME: assert remaining args is empty
fromList :: (a -> Int) -> [Annotated a] -> Ast a
fromList _ [] = undefined
fromList arity (x:xs) = buildAst args x 
  where (args, _) = argBuilder arity (arity' x) xs
        arity'    = arity . unAnnotate

argBuilder :: (a -> Int) -> Int -> [Annotated a] -> ([Ast a], [Annotated a])
argBuilder _ 0 xs          = ([], xs)
argBuilder _ _ []          = undefined
argBuilder arity n (x:xs)  = (args, remainder)
  where 
    args     = argsHead:argsTail
    argsHead = buildAst headArgs x
    (argsTail, remainder ) = argBuilder arity (n-1) remainder'
    (headArgs, remainder') = argBuilder arity (arity' x) xs
    arity'                 = arity . unAnnotate

-- TODO: Better time complexity
getVars :: Ast a -> [a]
getVars = \case
  Variable  a      -> [a]
  Function  f args -> f:(foldr (++) [] $ map getVars args)
  Predicate f args -> f:(foldr (++) [] $ map getVars args)

-- TODO: implement with fold instead?
stringifyAst :: (a -> String) -> Ast a -> String
stringifyAst stringify = \case
  Variable  v      -> stringify v
  Function  f args ->
    "(" ++ stringify f ++ " " ++ unwords (map (stringifyAst stringify) args) ++ ")"
  Predicate p args ->
    "(" ++ stringify p ++ " " ++ unwords (map (stringifyAst stringify) args) ++ ")"

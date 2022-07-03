-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.AST
-- Description :  Abstract Syntax Tree form for signals, functions, and predicates.
-- Maintainer  :  Wonhyuk Choi
-- Alternate data structure format for ADT's in TSL.Logic.
-- All signal/function/predicate Abstract Data Types in TSL.Logic are curried,
-- and they need to transformed to a AST-style syntax in order to be
-- fed into a SMT or SyGuS solver. 
-- This module defines the AST type, and the transformative functions thereof.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.AST( AST
                             , fromSignalTerm
                             , fromFunctionTerm
                             , fromPredicateTerm
                             ) where

-------------------------------------------------------------------------------

import TSL.Logic ( Formula(..)
                 , SignalTerm(..)
                 , FunctionTerm(..)
                 , PredicateTerm(..)
                 )

import TSL.Types(ExprType(..))

import TSL.Specification (Specification(..))

import TSL.SymbolTable (Id)

-------------------------------------------------------------------------------

data AST a = Function a [AST a]

instance Show a => Show (AST a) where
  show = \case
    (Function f []) -> show f
    (Function f xs) -> "(" ++ show f ++ " " ++ (unwords (map show xs)) ++ ")"

instance Functor AST where
    fmap f (Function a as) = Function (f a) (map (fmap f) as)

fromSignalTerm :: (a -> Int) -> SignalTerm a -> AST a
fromSignalTerm arity = (fromList arity) . flattenS

fromFunctionTerm :: (a -> Int) -> FunctionTerm a -> AST a
fromFunctionTerm arity = (fromList arity) . flattenF

fromPredicateTerm :: (a -> Int) -> PredicateTerm a -> AST a
fromPredicateTerm arity = (fromList arity) . flattenP

flattenS :: SignalTerm a -> [a]
flattenS = \case
  Signal a -> [a]
  FunctionTerm f  -> flattenF f
  PredicateTerm p -> flattenP p

flattenF :: FunctionTerm a -> [a]
flattenF = \case
  FunctionSymbol a -> [a]
  FApplied fterm s -> flattenF fterm ++ flattenS s

flattenP :: PredicateTerm a -> [a]
flattenP = \case
  BooleanTrue       -> undefined -- Not Implemented
  BooleanFalse      -> undefined -- Not Implemented
  BooleanInput a    -> [a]
  PredicateSymbol a -> [a]
  PApplied pterm s  -> flattenP pterm ++ flattenS s

fromList :: (a -> Int) -> [a] -> AST a
fromList _ []         = undefined
fromList arity (x:xs) = Function x args
  where (args, _) = argBuilder arity (arity x) xs

argBuilder :: (a -> Int) -> Int -> [a] -> ([AST a], [a])
argBuilder _ 0 xs          = ([], xs)
argBuilder _ _ []          = undefined
argBuilder arity n (x:xs)  = (args, remainder)
  where 
    args     = argsHead:argsTail
    argsHead = Function x headArgs
    (argsTail, remainder)  = argBuilder arity (n-1) remainder'
    (headArgs, remainder') = argBuilder arity (arity x) xs

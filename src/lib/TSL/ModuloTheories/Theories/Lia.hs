-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Lia
-- Description :  Linear Integer Arithmetic
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.Lia(LiaSymbol) where

-------------------------------------------------------------------------------

import Text.Regex.PCRE.Heavy (scan, re)

import TSL.ModuloTheories.Theories.Base (TheorySymbol(..))

-------------------------------------------------------------------------------

data LiaSymbol = 
    Int (Int)
  | Var (String)
  | Add
  | Sub
  | Eq
  | Gt
  | Lt
  | Gte
  | Lte
  deriving(Eq, Ord)

instance TheorySymbol LiaSymbol where
  readT = \case
    "add" -> Right Add
    "sub" -> Right Sub
    "eq"  -> Right Eq 
    "gt"  -> Right Gt 
    "lt"  -> Right Lt 
    "gte" -> Right Gte
    "lte" -> Right Lte
    value -> case scan [re|int([0-9]+)|] value of
               []          -> Right $ Var value
               [(_,[int])] -> Right $ Int $ read int
               _           -> error $ "Invalid: " ++ value

  toSmt = \case
    (Int i) -> show i
    (Var v) -> show v
    Add     -> "+"
    Sub     -> "-"
    Eq      -> "="
    Gt      -> ">"
    Lt      -> "<"
    Gte     -> ">="
    Lte     -> "<="

  toTsl = \case
    (Int i) -> show i
    (Var v) -> show v
    Add     -> "add"
    Sub     -> "sub"
    Eq      -> "eq"
    Gt      -> "gt"
    Lt      -> "lt"
    Gte     -> "gte"
    Lte     -> "lte"

  symbolType      _  = "Int"
  isUninterpreted _  = False

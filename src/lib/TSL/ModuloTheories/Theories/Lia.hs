-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Lia
-- Description :  Linear Integer Arithmetic
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.Lia(LiaSymbol) where

-------------------------------------------------------------------------------

import TSL.Error (errMtParse)

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
    "+"  -> Right Add
    "-"  -> Right Sub
    "="  -> Right Eq
    ">"  -> Right Gt
    "<"  -> Right Lt
    ">=" -> Right Gte
    "<=" -> Right Lte
    str  -> errMtParse str

  toSmt = \case
    (Int i) -> show i
    (Var v) -> show v
    Add     -> "+"
    Sub     -> "-"
    Eq      -> "->"
    Gt      -> ">"
    Lt      -> "<"
    Gte     -> ">->"
    Lte     -> "<->"

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

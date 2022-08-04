-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Lia
-- Description :  Linear Integer Arithmetic
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.Lia(LiaSymbol) where

-------------------------------------------------------------------------------

import TSL.Error (Error(..), errMtParse)

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

instance TheorySymbol LiaSymbol where
  readT "+"  = Right Add
  readT "-"  = Right Sub
  readT "="  = Right Eq
  readT ">"  = Right Gt
  readT "<"  = Right Lt
  readT ">=" = Right Gte
  readT "<=" = Right Lte
  readT str  = errMtParse str

  toSmt (Int i) = show i
  toSmt (Var v) = show v
  toSmt Add     = "+"
  toSmt Sub     = "-"
  toSmt Eq      = "="
  toSmt Gt      = ">"
  toSmt Lt      = "<"
  toSmt Gte     = ">="
  toSmt Lte     = "<="

  toTsl (Int i) = show i
  toTsl (Var v) = show v
  toTsl Add     = "add"
  toTsl Sub     = "sub"
  toTsl Eq      = "eq"
  toTsl Gt      = "gt"
  toTsl Lt      = "lt"
  toTsl Gte     = "gte"
  toTsl Lte     = "lte"

  symbolType _  = "Int"

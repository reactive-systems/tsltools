-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Lia
-- Description :  Linear Integer Arithmetic
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.Lia(LiaSymbol) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , TheorySymbol(..)
                                       )

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
  readT _    = Left TheoryParseErr

  toSMT2 (Int i) = show i
  toSMT2 (Var v) = show v
  toSMT2 Add     = "+"
  toSMT2 Sub     = "-"
  toSMT2 Eq      = "="
  toSMT2 Gt      = ">"
  toSMT2 Lt      = "<"
  toSMT2 Gte     = ">="
  toSMT2 Lte     = "<="

  toTSL (Int i) = show i
  toTSL (Var v) = show v
  toTSL Add     = "add"
  toTSL Sub     = "sub"
  toTSL Eq      = "eq"
  toTSL Gt      = "gt"
  toTSL Lt      = "lt"
  toTSL Gte     = "gte"
  toTSL Lte     = "lte"

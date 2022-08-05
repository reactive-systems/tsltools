-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeFamilies    #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TAst
                                  , readTheory
                                  , applySemantics
                                  , tastTheory
                                  , tast2Tsl
                                  , tast2Smt
                                  ) where
-------------------------------------------------------------------------------

import TSL.Error (Error, errMtParse)

import TSL.Ast(Ast, stringifyAst)

import qualified TSL.ModuloTheories.Theories.Base as Base(TheorySymbol(..))

import qualified TSL.ModuloTheories.Theories.Uf as Uf(UfSymbol)
import qualified TSL.ModuloTheories.Theories.Lia as Lia(LiaSymbol)

-------------------------------------------------------------------------------

data Theory = 
      Uf
    | Lia

instance Show Theory where
  show = \case
    Uf  -> "UF"
    Lia -> "LIA"

readTheory :: String -> Either Error Theory
readTheory "#UF"  = Right Uf
readTheory "#LIA" = Right Lia
readTheory other  = errMtParse other

data TAst =
    UfAst  (Ast Uf.UfSymbol)
  | LiaAst (Ast Lia.LiaSymbol)

instance Show TAst where show = tast2Smt

tastTheory :: TAst -> Theory
tastTheory (UfAst  _) = Uf
tastTheory (LiaAst _) = Lia

tast2Tsl :: TAst -> String
tast2Tsl (UfAst  ast) = stringifyAst Base.toTsl ast
tast2Tsl (LiaAst ast) = stringifyAst Base.toTsl ast

tast2Smt :: TAst -> String
tast2Smt (UfAst  ast) = stringifyAst Base.toSmt ast
tast2Smt (LiaAst ast) = stringifyAst Base.toSmt ast

applySemantics :: Theory -> Ast String -> Either Error TAst
applySemantics Uf  ast = UfAst  <$> traverse Base.readT ast
applySemantics Lia ast = LiaAst <$> traverse Base.readT ast

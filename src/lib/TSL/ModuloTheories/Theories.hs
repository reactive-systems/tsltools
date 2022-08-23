-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi
-- One may wonder why there is a separate TAst data structure instead of 
-- using `Ast TheorySymbol`.
-- This is because on data value level, there is no way to 
-- enforce relationships between `Theory` and `TheorySymbol`.
-- Instead, we use TAst to hold information about the Theory
-- as well as the actual Abstract Syntax Tree.
-- This means there is a lot more boilerplate code for adding
-- a new theory, but it achieves type safety.

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeFamilies    #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TAst
                                  , TheorySymbol
                                  , readTheory
                                  , smtSortDecl
                                  , applySemantics
                                  , tastTheory
                                  , tast2Tsl
                                  , tast2Smt
                                  , tastInfo
                                  , symbol2Tsl
                                  , symbol2Smt
                                  , symbolType
                                  , isUninterpreted
                                  ) where
-------------------------------------------------------------------------------

import TSL.Error (Error, errMtParse)

import TSL.Ast( Ast
              , AstInfo
              , stringifyAst
              , getAstInfo
              , astByDepth
              )

import qualified TSL.ModuloTheories.Theories.Base as Base(TheorySymbol(..))

import qualified TSL.ModuloTheories.Theories.Uf as Uf(UfSymbol)
import qualified TSL.ModuloTheories.Theories.EUf as EUf(EUfSymbol)
import qualified TSL.ModuloTheories.Theories.Lia as Lia(LiaSymbol)

-------------------------------------------------------------------------------

data Theory = 
      Uf
    | EUf
    | Lia
    deriving(Eq)

instance Show Theory where
  show = \case
    Uf  -> "UF"
    EUf -> "UF"
    Lia -> "LIA"

readTheory :: String -> Either Error Theory
readTheory "#UF"  = Right Uf
readTheory "#EUF" = Right EUf
readTheory "#LIA" = Right Lia
readTheory other  = errMtParse other

smtSortDecl :: Theory -> String
smtSortDecl = \case
  Uf  -> "(declare-sort UF 0)"
  EUf -> "(declare-sort UF 0)"
  Lia -> ""

data TAst =
    UfAst  (Ast Uf.UfSymbol)
  | EUfAst (Ast EUf.EUfSymbol)
  | LiaAst (Ast Lia.LiaSymbol)

instance Show TAst where show = tast2Smt

tastByDepth :: TAst -> [TAst]
tastByDepth (UfAst  ast) = map UfAst  $ astByDepth ast 
tastByDepth (EUfAst ast) = map EUfAst $ astByDepth ast
tastByDepth (LiaAst ast) = map LiaAst $ astByDepth ast

tastTheory :: TAst -> Theory
tastTheory (UfAst  _) = Uf
tastTheory (EUfAst _) = EUf
tastTheory (LiaAst _) = Lia

tast2Tsl :: TAst -> String
tast2Tsl (UfAst  ast) = stringifyAst Base.toTsl ast
tast2Tsl (EUfAst ast) = stringifyAst Base.toTsl ast
tast2Tsl (LiaAst ast) = stringifyAst Base.toTsl ast

tast2Smt :: TAst -> String
tast2Smt (UfAst  ast) = stringifyAst Base.toSmt ast
tast2Smt (EUfAst ast) = stringifyAst Base.toSmt ast
tast2Smt (LiaAst ast) = stringifyAst Base.toSmt ast

applySemantics :: Theory -> Ast String -> Either Error TAst
applySemantics Uf  ast = UfAst  <$> traverse Base.readT ast
applySemantics EUf ast = EUfAst <$> traverse Base.readT ast
applySemantics Lia ast = LiaAst <$> traverse Base.readT ast

data TheorySymbol = 
    UfSymbol  Uf.UfSymbol
  | EUfSymbol EUf.EUfSymbol
  | LiaSymbol Lia.LiaSymbol
  deriving(Eq)

tastInfo :: TAst -> AstInfo TheorySymbol
tastInfo = \case
  UfAst ast  -> fmap UfSymbol  $ getAstInfo ast
  EUfAst ast -> fmap EUfSymbol $ getAstInfo ast
  LiaAst ast -> fmap LiaSymbol $ getAstInfo ast

symbol2Tsl :: TheorySymbol -> String
symbol2Tsl (UfSymbol  symbol) = Base.toTsl symbol
symbol2Tsl (EUfSymbol symbol) = Base.toTsl symbol
symbol2Tsl (LiaSymbol symbol) = Base.toTsl symbol

symbol2Smt :: TheorySymbol -> String
symbol2Smt (UfSymbol  symbol) = Base.toSmt symbol
symbol2Smt (EUfSymbol symbol) = Base.toSmt symbol
symbol2Smt (LiaSymbol symbol) = Base.toSmt symbol

isUninterpreted :: TheorySymbol -> Bool
isUninterpreted (UfSymbol  symbol) = Base.isUninterpreted symbol
isUninterpreted (EUfSymbol symbol) = Base.isUninterpreted symbol
isUninterpreted (LiaSymbol symbol) = Base.isUninterpreted symbol

symbolTheory :: TheorySymbol -> Theory
symbolTheory (UfSymbol  _) = Uf
symbolTheory (EUfSymbol _) = EUf
symbolTheory (LiaSymbol _) = Lia

symbolType :: TheorySymbol -> String
symbolType = show . symbolTheory

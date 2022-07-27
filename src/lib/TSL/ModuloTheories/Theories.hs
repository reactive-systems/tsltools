-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TheorySymbol(..)
                                  , readTheory
                                  , applySemantics
                                  , toSmt
                                  , toTsl
                                  , symbolType
                                  ) where
-------------------------------------------------------------------------------

import TSL.Ast(Ast)

import TSL.ModuloTheories.Theories.Base(TheoryParseErr(..))
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

data TheorySymbol = 
      UfSymbol  Uf.UfSymbol
    | LiaSymbol Lia.LiaSymbol

toTsl :: TheorySymbol -> String
toTsl (UfSymbol  uf)  = Base.toTsl uf
toTsl (LiaSymbol lia) = Base.toTsl lia

toSmt :: TheorySymbol -> String
toSmt (UfSymbol  uf)  = Base.toSmt uf
toSmt (LiaSymbol lia) = Base.toSmt lia

symbolType :: TheorySymbol -> String
symbolType (UfSymbol   uf) = Base.symbolType uf
symbolType (LiaSymbol lia) = Base.symbolType lia

readTheory :: String -> Either TheoryParseErr Theory
readTheory "UF"  = Right Uf
readTheory "LIA" = Right Lia
readTheory _     = Left TheoryParseErr

applySemantics :: Theory -> Ast String -> Either TheoryParseErr (Ast TheorySymbol)
applySemantics Uf  ast = fmap (fmap UfSymbol ) $ traverse Base.readT ast
applySemantics Lia ast = fmap (fmap LiaSymbol) $ traverse Base.readT ast

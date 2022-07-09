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
                                  , toSMT2
                                  , toTSL
                                  ) where
-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories.Base(TheoryParseErr(..))
import qualified TSL.ModuloTheories.Theories.Base as Base(readT, toSMT2, toTSL)

import qualified TSL.ModuloTheories.Theories.Uf as Uf(UfSymbol(..))
import qualified TSL.ModuloTheories.Theories.Lia as Lia(LiaSymbol(..))

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

toTSL :: TheorySymbol -> String
toTSL (UfSymbol  uf)  = Base.toTSL uf
toTSL (LiaSymbol lia) = Base.toTSL lia

toSMT2 :: TheorySymbol -> String
toSMT2 (UfSymbol  uf)  = Base.toSMT2 uf
toSMT2 (LiaSymbol lia) = Base.toSMT2 lia

readTheory :: String -> Either TheoryParseErr Theory
readTheory "UF"  = Right Uf
readTheory "LIA" = Right Lia
readTheory _     = Left TheoryParseErr

applySemantics :: Theory -> String -> Either TheoryParseErr TheorySymbol
applySemantics Uf  a = Base.readT a >>= (Right . UfSymbol)
applySemantics Lia a = Base.readT a >>= (Right . LiaSymbol)

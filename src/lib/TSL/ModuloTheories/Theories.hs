-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TheorySymbol(..)
                                  , readTheory
                                  -- , applySemantics
                                  ) where
-------------------------------------------------------------------------------

-- import TSL.ModuloTheories.AST(AST(..))

import TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , TheorySymbol(..)
                                       )

import TSL.ModuloTheories.Theories.Uf(UfTheory(..))
import TSL.ModuloTheories.Theories.Lia(LiaTheory(..))

-------------------------------------------------------------------------------
data Theory = 
      Uf  UfTheory
    | Lia LiaTheory
    deriving (Show)

-- applySemantics :: (TheorySymbol b) => TTheory -> AST String -> AST (Either TheoryParseErr b)
-- applySemantics (Uf t) = applySemantics t

readTheory :: String -> Either TheoryParseErr Theory
readTheory "UF"  = Right $ Uf UfTheory
readTheory "LIA" = Right $ Lia LiaTheory
readTheory _     = Left TheoryParseErr

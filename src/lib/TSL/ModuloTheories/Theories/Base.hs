-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.Base
-- Description :  Typeclass declarations for all theories
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , Theory(..)
                                       , TheorySymbol(..)
                                       ) where
-------------------------------------------------------------------------------

import TSL.ModuloTheories.AST(AST(..))

-------------------------------------------------------------------------------

data TheoryParseErr = TheoryParseErr deriving (Show)

class Theory t where
    type Symbol t
    applySemantics :: t -> AST String -> AST (Either TheoryParseErr (Symbol t))

class TheorySymbol a where
  readT  :: String -> Either TheoryParseErr a
  toSMT2 :: a -> String
  toTSL  :: a -> String

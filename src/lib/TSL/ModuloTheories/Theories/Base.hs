-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.Base
-- Description :  Typeclass declarations for all theories
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE QuantifiedConstraints #-}
-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , Theory
                                       , TheorySymbol
                                       , readT
                                       , applySemantics
                                       , toSMT2
                                       , toTSL
                                       ) where
-------------------------------------------------------------------------------

import TSL.ModuloTheories.AST(AST(..))

-------------------------------------------------------------------------------

data TheoryParseErr = TheoryParseErr deriving (Show)

class Theory t where
    applySemantics
        :: (forall s. TheorySymbol s)
        => t
        -> AST String
        -> AST (Either TheoryParseErr s)

class TheorySymbol a where
  readT  :: String -> Either TheoryParseErr a
  toSMT2 :: a -> String
  toTSL  :: a -> String

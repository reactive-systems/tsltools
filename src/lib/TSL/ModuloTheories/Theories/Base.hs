-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.Base
-- Description :  Typeclass declarations for all theories
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , TheorySymbol(..)
                                       ) where
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

data TheoryParseErr = TheoryParseErr deriving (Show)

class TheorySymbol a where
  readT      :: String -> Either TheoryParseErr a
  toSmt      :: a -> String
  toTsl      :: a -> String
  symbolType :: a -> String

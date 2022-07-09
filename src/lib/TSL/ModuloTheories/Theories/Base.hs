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

import TSL.ModuloTheories.AST(AST(..))

-------------------------------------------------------------------------------

data TheoryParseErr = TheoryParseErr deriving (Show)

class TheorySymbol a where
  readT  :: String -> Either TheoryParseErr a
  toSMT2 :: a -> String
  toTSL  :: a -> String

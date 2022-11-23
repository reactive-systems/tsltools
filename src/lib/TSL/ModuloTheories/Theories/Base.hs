-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- |
-- Module      :  TSL.ModuloTheories.Theories.Base
-- Description :  Typeclass declarations for all theories
-- Maintainer  :  Wonhyuk Choi
module TSL.ModuloTheories.Theories.Base (TheorySymbol (..)) where

-------------------------------------------------------------------------------

import TSL.Error (Error)

-------------------------------------------------------------------------------

class TheorySymbol a where
  readT :: String -> Either Error a
  toSmt :: a -> String
  toTsl :: a -> String
  symbolType :: a -> String
  isUninterpreted :: a -> Bool
  makeSignal :: String -> a

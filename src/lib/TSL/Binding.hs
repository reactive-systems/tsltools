-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Binding
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- A data type to store an identifier bound to an expression.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.Binding
  ( Binding(..)
  , BoundExpr(..)
  ) where

-----------------------------------------------------------------------------

import TSL.Expression
  ( Expr
  , ExprPos
  )

import Control.Arrow
  ( (***)
  )

-----------------------------------------------------------------------------

data BoundExpr a =
    GuardedBinding [Expr a]
  | PatternBinding (Expr a) (Expr a)
  | SetBinding (Expr a)
  | RangeBinding (Expr a) (Int -> Int) (Expr a) (Int -> Int)

-----------------------------------------------------------------------------

instance Functor BoundExpr where
  fmap f = \case
    GuardedBinding xs    -> GuardedBinding $ map (fmap f) xs
    PatternBinding x y   -> PatternBinding (fmap f x) $ fmap f y
    SetBinding x         -> SetBinding $ fmap f x
    RangeBinding x g y h -> RangeBinding (fmap f x) g (fmap f y) h

-----------------------------------------------------------------------------

-- | The data type @Bind a@ expresses a binding of some instance of
-- type @a@ to some expression. The identifiers inside this expression
-- need to be represented by instances of type @a@ as well. Finally, a
-- binding also containts the source position of the bound identifier
-- as well as possible arguments in case the bindings represents a
-- function.

data Binding a =
  Binding
    { bIdent :: a
    , bArgs :: [(a, ExprPos)]
    , bPos :: ExprPos
    , bVal :: BoundExpr a
    }

-----------------------------------------------------------------------------

instance Functor Binding where
  fmap f Binding{..} =
    Binding
      { bIdent = f bIdent
      , bArgs = map (f *** id) bArgs
      , bPos = bPos
      , bVal = fmap f bVal
      }

-----------------------------------------------------------------------------

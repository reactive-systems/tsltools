-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Reader.Sugar
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Removes syntactic sugar elements from the specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

module TSL.Reader.Sugar
  ( replaceSugar
  ) where

-----------------------------------------------------------------------------

import TSL.Error
  ( Error
  )

import TSL.Binding
  ( Binding(..)
  , BoundExpr(..)
  )

import TSL.Reader.Data
  ( Specification(..)
  )

import TSL.Expression
  ( Expr(..)
  , Expr'(..)
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

-- | Replaces syntactic sugar elements in the given specification by their
-- corresponding standard elements.

replaceSugar
  :: Specification -> Either Error Specification

replaceSugar s = do
  vs <- mapM replaceBinding $ definitions s
  return s { definitions = vs }

-----------------------------------------------------------------------------

replaceBinding
  :: Binding Int -> Either Error (Binding Int)

replaceBinding b =
  return b
    { bVal = case bVal b of
        GuardedBinding xs      -> GuardedBinding $ replaceExpr xs
        PatternBinding x y     -> PatternBinding (rpE x) (rpE y)
        SetBinding x           -> SetBinding $ rpE x
        RangeBinding x f1 y f2 -> RangeBinding (rpE x) f1 (rpE y) f2
    }

  where
    rpE x = head $ replaceExpr [x]

-----------------------------------------------------------------------------

replaceExpr
  :: [Expr Int] -> [Expr Int]

replaceExpr = \case
  []  -> assert False undefined
  [e] -> replaceOtherwise [] [] $ checkForFn e
  xs  -> replaceOtherwise [] [] xs

  where
    replaceOtherwise a b = \case
      []   -> reverse a
      e:er -> case expr e of
        Colon x y -> case expr x of
          BaseOtherwise ->
            let c = Expr (BlnNot (orList (srcPos e) b)) (-1) $ srcPos e
            in reverse $ (e { expr = Colon c y }) : a
          _             ->
            replaceOtherwise (e:a) (x:b) er
        _         ->
          let c = Expr (BlnNot (orList (srcPos e) b)) (-1) $ srcPos e
          in reverse $ (e { expr = Colon c e }) : a

    checkForFn e = case expr e of
      BaseFn x y -> checkFn e x y
      Colon {}   -> [e]
      _          -> [e { expr = Colon (Expr BaseOtherwise (-1) $ srcPos e) e }]

    checkFn e x y = case expr x of
      Colon {} -> x : checkForFn y
      _        -> [e]

    orList p =
      foldl (fldOr p) (Expr BaseFalse (-1) p)

    fldOr p e1 e2 =
      Expr (BlnOr e1 e2) (-1) p

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Reader.Data
-- Maintainer  :  Felix Klein
--
-- Common data used by the 'Reader' module.
--
-----------------------------------------------------------------------------

module TSL.Reader.Data
  ( NameTable
  , PositionTable
  , ArgumentTable
  , ExpressionTable
  , TypeTable
  , DependencyTable
  , ScopeTable
  , Specification(..)
  ) where

-----------------------------------------------------------------------------

import TSL.Types (ExprType, SectionType)

import TSL.Expression (Expr, ExprPos)

import TSL.Binding (Binding, BoundExpr)

import Data.IntMap.Strict (IntMap)

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to their correpsonding names.

type NameTable = IntMap String

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to their defined position in the
-- source file.

type PositionTable = IntMap ExprPos

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to their arguments, in case the bound
-- expression is a function (only for globally bound identifiers).

type ArgumentTable = IntMap [Int]

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to their bound expression (only for
-- globally bound identifiers). An expression may consist of multiple
-- sub-expressions, if guarded by different patterns.

type ExpressionTable = IntMap (BoundExpr Int)

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to their correspoinging type.

type TypeTable = IntMap ExprType

-----------------------------------------------------------------------------

-- | Mapping that maps identifiers to the identifiers it depends on.

type DependencyTable = IntMap [Int]

-----------------------------------------------------------------------------

-- | Mapping that maps all identifiers to unit that are bound
-- internally by the specification and not externally through the
-- logic.
type ScopeTable = IntMap ()

-----------------------------------------------------------------------------

-- | The internal representation of a specification used by the reader
-- module.

data Specification =
  Specification
  { -- | The bindings of identifiers to expressions that are defined
    --  globally.
    definitions :: [Binding Int]
  , -- | The list of sections elements, each containing the list
    -- of expressions for that respective section.
    sections :: [(SectionType, Expr Int)]
  , -- | The id to bounded-expression mapping.
    bindings :: ExpressionTable
  , -- | The id to name mapping.
    names :: NameTable
  , -- | The id to source position mapping.
    positions :: PositionTable
  , -- | The id to arguments mapping.
    arguments :: ArgumentTable
  , -- | The id to depending ids mapping.
    dependencies :: DependencyTable
  , -- | The id to type of the bound expression mapping.
    types :: TypeTable
  , -- | The id to type of the bound expression mapping.
    scopes :: ScopeTable
  , -- | Number of different expressions in the input.
    exprRange :: (Int,Int)

  }

-----------------------------------------------------------------------------

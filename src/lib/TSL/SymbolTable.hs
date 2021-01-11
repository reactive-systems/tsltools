-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.SymbolTable
-- Maintainer  :  Felix Klein
--
-- Data type to store all identifier specific content.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

-----------------------------------------------------------------------------

module TSL.SymbolTable
  ( SymbolTable(..)
  , IdRec(..)
  , Kind(..)
  , toCSV
  , symbolTable
  , csvFormat
  ) where

-----------------------------------------------------------------------------

import Data.List (sortBy, transpose)

import TSL.Types (ExprType(..), prType)

import TSL.Expression (ExprPos(..), SrcPos(..))

import TSL.Binding (BoundExpr)

import Data.Char (toLower)

import Data.Array (Array, assocs, bounds, (!))

import Control.Monad.ST (ST, runST)

import qualified Data.IntMap.Strict as IM (fromList, member, (!))

import Control.Exception (assert)

import Data.Array.ST (STArray, newArray, readArray, writeArray)

import Data.Set (fromList, toList)

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

-----------------------------------------------------------------------------

type Id = Int

-----------------------------------------------------------------------------

data Kind =
  Input | Output | Constant | Predicate | Function | Internal
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

data SymbolTable =
  SymbolTable
    { symtable :: Array Id IdRec
    , stBounds :: (Id, Id)
    , stName :: Id -> String
    , stPos :: Id -> Maybe ExprPos
    , stArgs ::  Id -> [Id]
    , stBindings :: Id -> Maybe (BoundExpr Id)
    , stType :: Id -> ExprType
    , stDeps :: Id -> [Id]
    , stKind :: Id -> Kind
    }

-----------------------------------------------------------------------------

symbolTable
  :: Array Int IdRec -> SymbolTable

symbolTable a =
  SymbolTable
    { symtable = a
    , stBounds = bounds a
    , stName = idName . (a !)
    , stPos = idPos . (a !)
    , stArgs = idArgs . (a !)
    , stBindings = idBindings . (a !)
    , stType = idType . (a !)
    , stDeps = idDeps . (a !)
    , stKind = idKind . (a !)
    }

-----------------------------------------------------------------------------

-- | Data type representing a single entry in the symbol table.

data IdRec =
  IdRec
    { -- | The name of the identifier.
      idName :: String
    , -- | The type of the identifier.
      idType :: ExprType
    , -- | Categorization to distinguish between globally or locally
      -- bound indentifiers and functions, predicates, input or output
      -- signals.
      idKind :: Kind
    , -- | The list of identifiers, which have to be evaluated first
      -- to evaluate this identifier.
      idDeps :: [Id]
    , -- | The arguemnts, in case the identifier describes a function.
      idArgs :: [Id]
    , -- | The position of the identifer definition in the source file.
      idPos :: Maybe ExprPos
    , -- | The expression, the identifier is bound to.
      idBindings :: Maybe (BoundExpr Id)
    }

-----------------------------------------------------------------------------

-- | Prints the symbol table in the CVS format.

toCSV
  :: SymbolTable -> String

toCSV SymbolTable{..} =
  let
    es = sortBy cmp $ assocs symtable
    ts = rmDouble $ concatMap polyType es
    im = IM.fromList $ zip ts [0,1..length ts-1]
    ir = IM.fromList $ zip (map fst es) [0,1..length es-1]
    sc = not $ null $ mapMaybe srcPath $ mapMaybe (idPos . snd) es
  in
    csvFormat
    $ ( filter (/="")
          [ "Id"
          , "Name"
          , "Type"
          , "Kind"
          , "Depends"
          , "Position"
          , if sc then "Source" else ""
          , "Arguments"
          ]
      :)
    $ map (printEntry sc
            (\i -> assert (IM.member i ir) (ir IM.! i))
            (\i -> assert (IM.member i im) (im IM.! i))) es

  where
    cmp (_,ir1@(idKind -> k1)) (_,ir2@(idKind -> k2))
      | k1 /= k2   = compare k1 k2
      | otherwise = compare (idName ir1) (idName ir2)

    printEntry sc g f (i, IdRec{..}) =
      catMaybes
        [ Just $ show (g i)
        , Just idName
        , Just $ prType f idType
        , Just $ map toLower $ show idKind
        , Just $ commasepxs $ toList $ fromList $ map g idDeps
        , Just $ prExprPos idPos
        , if sc then Just $ prSource idPos else Nothing
        , Just $ commasepxs idArgs
        ]

    polyType (_,IdRec{..}) =
      pType [] idType

    pType a = \case
      TPoly i   -> i:a
      TSignal i -> pType a i
      TFml x y  -> pType (pType a y) x
      _         -> a

    commasepxs = \case
      (x:xr) -> show x ++ concatMap ((',':) . (' ':) . show) xr
      []     -> ""

    prSource = \case
      Nothing          -> ""
      Just ExprPos{..} ->
        fromMaybe "" srcPath

    prExprPos = \case
      Nothing          -> ""
      Just ExprPos{..} ->
        show (srcLine srcBegin) ++ "," ++ show (srcColumn srcBegin) ++
        ( if srcLine srcBegin == srcLine srcEnd
          then "-"
          else show (srcLine srcEnd) ++ ","
        ) ++
        show (srcColumn srcEnd)

-----------------------------------------------------------------------------

-- | Removes double entries of an 'Int' list without distorting the
-- order by using a lookup table.

rmDouble
  :: [Int] -> [Int]

rmDouble xs =
  let
    max_v = maximum xs
    min_v = minimum xs
  in runST $ do
    a <- newArray (min_v, max_v) False
    ys <- mapM (markDouble a) xs
    return $ map fst $ filter snd ys

  where
    markDouble
      :: STArray s Int Bool -> Int -> ST s (Int, Bool)

    markDouble a x = do
      y <- readArray a x
      if y
      then return (x, False)
      else do
        writeArray a x True
        return (x, True)

-----------------------------------------------------------------------------

-- | Layouts the CSV table by adding whitespaces to be more easy to
-- read.

csvFormat
  :: [[String]] -> String

csvFormat xs =
  let
    -- get the maximal length of all entries for each column
    ms = map (maximum . map length) $ transpose xs
    -- fill smaller colums with withspaces
    zs = map (map fill . zip3 [0,1..length ms-1] ms) xs
  in
    -- add the CSV separators
    unlines $ map csvSep zs

-----------------------------------------------------------------------------

-- | CSV table entries are printed as a semicolon separated list.

csvSep
  :: [String] -> String

csvSep = \case
  (x:xr) -> x ++ concatMap ((' ':) . (';':) . (' ':)) xr
  []     -> ""

-----------------------------------------------------------------------------

-- | Fills up strings 'x' of length 'i' by white spaces up to the
-- length 'n'.

fill
  :: (Int, Int, String) -> String

fill (i, n, x)
  | length x >= n = x
  | i == 0        = replicate (n - length x) ' ' ++ x
  | otherwise    = x ++ replicate (n - length x) ' '

-----------------------------------------------------------------------------

{-
prPrettyExpr
  :: SymbolTable -> Expr Int -> String

prPrettyExpr _  (expr -> SetExplicit []) = ""
prPrettyExpr st e = pr e

  where
    pr = pr' . expr

    pr' = \case
      BaseWild         -> "_"
      BaseTrue         -> "\"true\""
      BaseFalse        -> "\"false\""
      BaseOtherwise    -> "otherwise"
      BaseCon x        -> "\"" ++ show x ++ "\""
      BaseId x         -> "\"" ++ idName (st ! x) ++ "\"<" ++ show x ++ ">"
      BaseUpd y x      -> "\"[" ++ idName (st ! x) ++ " <- " ++ pr y ++ "]\"<" ++ show x ++ ">"
      BaseFml xs y     -> "\"" ++ idName (st ! y) ++ "\"<" ++ show y ++ ">(" ++
                          (if null xs then ""
                           else pr (head xs) ++
                                concatMap ((:) ',' . pr) (tail xs)) ++ ")"
      BaseFn xs y      -> "\"" ++ idName (st ! y) ++ "\"<" ++ show y ++ "> " ++
                          (if null xs then ""
                           else pr (head xs) ++
                                concatMap ((:) ' ' . pr) (tail xs))
      NumSMin x        -> "min " ++ pr x
      NumSMax x        -> "max " ++ pr x
      NumSSize x       -> "|" ++ pr x ++ "|"
      BlnNot x         -> "¬" ++ pr x
      TslNext x        -> "X " ++ pr x
      TslGlobally x    -> "G " ++ pr x
      TslFinally x     -> "F " ++ pr x
      NumPlus x y      -> pr x ++ " + " ++ pr y
      NumMinus x y     -> pr x ++ " - " ++ pr y
      NumMul x y       -> pr x ++ " * " ++ pr y
      NumDiv x y       -> pr x ++ " / " ++ pr y
      NumMod x y       -> pr x ++ " % " ++ pr y
      SetCup (expr -> SetExplicit []) x -> pr x
      SetCup x (expr -> SetExplicit []) -> pr x
      SetCup x y                       -> pr x ++ " ∪ " ++ pr y
      SetCap (expr -> SetExplicit []) x -> pr x
      SetCap x (expr -> SetExplicit []) -> pr x
      SetCap x y                       -> pr x ++ " ∩ " ++ pr y
      SetMinus x y     -> pr x ++ " ∖ " ++ pr y
      BlnEQ x y        -> pr x ++ " = " ++ pr y
      BlnNEQ x y       -> pr x ++ " ≠ " ++ pr y
      BlnGE x y        -> pr x ++ " > " ++ pr y
      BlnGEQ x y       -> pr x ++ " ≥ " ++ pr y
      BlnLE x y        -> pr x ++ " < " ++ pr y
      BlnLEQ x y       -> pr x ++ " < " ++ pr y
      BlnElem x y      -> pr x ++ " ≤ " ++ pr y
      BlnOr x y        -> pr x ++ " ∨ " ++ pr y
      BlnAnd x y       -> pr x ++ " ∧ " ++ pr y
      BlnImpl x y      -> pr x ++ " → " ++ pr y
      BlnEquiv x y     -> pr x ++ " ↔ " ++ pr y
      TslRNext x y     -> "X [ " ++ pr x ++ " ] " ++ pr y
      TslRGlobally x y -> "G [ " ++ pr x ++ " ] " ++ pr y
      TslRFinally x y  -> "F [ " ++ pr x ++ " ] " ++ pr y
      TslUntil x y     -> pr x ++ " U " ++ pr y
      TslWeak x y      -> pr x ++ " W " ++ pr y
      TslAsSoonAs x y  -> pr x ++ " A " ++ pr y
      TslRelease x y   -> pr x ++ " R " ++ pr y
      Colon x y        -> pr x ++ " : " ++ pr y
      Pattern x y      -> pr x ++ " ~ " ++ pr y
      SetRange x y z   -> "[ " ++ pr x ++ ", " ++ pr y ++ " .. " ++ pr z ++ " ]"
      SetExplicit []     -> "∅"
      SetExplicit [x]    -> "{ " ++ pr x ++ " }"
      SetExplicit (x:xr) -> "{ " ++ pr x ++ concatMap ((',':) . (' ':) . pr) xr ++ "}"
      NumRPlus xs x    -> "Σ [ " ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      NumRMul xs x     -> "Π [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      SetRCup xs x     -> "⋃ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      SetRCap xs x     -> "⋂ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      BlnROr xs x      -> "⋁ [" ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
      BlnRAnd xs x     -> "⋀ [ " ++ concatMap (flip (++) " " . pr) xs ++ "] " ++ pr x
-}
-----------------------------------------------------------------------------

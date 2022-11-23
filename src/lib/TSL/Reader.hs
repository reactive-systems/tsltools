-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.Reader
-- Maintainer  :  Felix Klein
--
-- The module reads a specification to the internal format.
module TSL.Reader
  ( fromTSL,
  )
where

-----------------------------------------------------------------------------

import Control.Arrow (second)
import Control.Exception (assert)
import Control.Monad ((>=>))
import qualified Data.Array.IArray as A (Array, array, (!))
import Data.Function (on)
import Data.Graph (buildG, topSort, transposeG)
import qualified Data.IntMap as IM
  ( IntMap,
    fromList,
    keys,
    lookup,
    maxViewWithKey,
    member,
    minViewWithKey,
    null,
    toAscList,
    (!),
  )
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, empty, insert, member, toList)
import System.Directory (canonicalizePath, doesFileExist, doesPathExist)
import System.FilePath.Posix (combine, isAbsolute, takeDirectory)
import TSL.Binding (Binding (..), BoundExpr (..))
import TSL.Error (Error, errCircularImp, genericError)
import TSL.Eval (eval)
import TSL.Expression
  ( Expr (..),
    Expr' (..),
    ExprPos (..),
    applySub,
    subExpressions,
  )
import TSL.Logic (Formula (..))
import TSL.Parser (parse)
import qualified TSL.Parser.Data as PD (Specification (..))
import TSL.Reader.Abstraction (abstract)
import TSL.Reader.Bindings (specBindings)
import qualified TSL.Reader.Data as RD (Specification (..))
import TSL.Reader.InferType (inferTypes)
import TSL.Reader.Sugar (replaceSugar)
import TSL.Specification (Specification (..))
import TSL.SymbolTable (IdRec (..), Kind (..), SymbolTable, symbolTable)
import TSL.Types (ExprType (..), SectionType (..))

-----------------------------------------------------------------------------

-- | Parses a TSL specification.
fromTSL ::
  Maybe FilePath -> String -> IO (Either Error Specification)
fromTSL specPath =
  resolveImports specPath [] >=> return . (>>= process)

-----------------------------------------------------------------------------

process ::
  PD.Specification -> Either Error Specification
process =
  -- replace variable names by a unique identifier
  abstract
    >=>
    -- replace syntactic sugar constructs for later converison
    replaceSugar
    >=>
    -- retrieve the bindings of expression variables
    specBindings
    >=>
    -- infer types and typecheck
    inferTypes
    >=>
    -- lift reader specification to global specification
    \s@RD.Specification {..} -> do
      let st = symtable s
      es <- eval st $ map snd sections
      return
        Specification
          { assumptions =
              [ initiate (st, f)
                | (st, f) <- zip (map fst sections) es,
                  assumption st
              ],
            guarantees =
              [ initiate (st, f)
                | (st, f) <- zip (map fst sections) es,
                  not (assumption st)
              ],
            symboltable = st
          }
  where
    assumption = \case
      InitiallyAssume -> True
      Assume {} -> True
      AlwaysAssume {} -> True
      _ -> False

--------------------------------------------------------------------------------

resolveImports ::
  Maybe FilePath -> [(FilePath, ExprPos)] -> String -> IO (Either Error PD.Specification)
resolveImports specPath ls str = case parse str of
  Left err -> return $ Left err
  Right spec -> loadImports spec
  where
    loadImports spec = case PD.imports spec of
      [] -> return $ Right spec
      (path, name, p1, _) : xr ->
        resolvePath path
          >>= \case
            Nothing ->
              return $
                genericError $
                  "Import path resolution failed: import path was: \"" ++ path ++ "\" in \"" ++ importer ++ "\""
            Just path' ->
              if any ((== path') . fst) ls
                then
                  let (x, p) : _ = filter ((== path') . fst) ls
                   in return $ errCircularImp [(path', p1), (x, p)] p
                else do
                  exists <- doesFileExist path'
                  if not exists
                    then
                      return $
                        genericError $
                          "Imported file does not exist \"" ++ path' ++ "\" (import path was: \"" ++ path ++ "\" in \"" ++ importer ++ "\")"
                    else
                      readFile path'
                        >>= resolveImports (Just path') ((path', p1 {srcPath = Just path'}) : ls)
                        >>= \case
                          Left err -> return $ Left err
                          Right spec' ->
                            loadImports
                              PD.Specification
                                { imports = xr,
                                  definitions =
                                    map
                                      (updB path' . fmap ((name ++ ".") ++))
                                      (PD.definitions spec')
                                      ++ PD.definitions spec,
                                  sections =
                                    map
                                      (second (upd path' . fmap ((name ++ ".") ++)))
                                      (PD.sections spec')
                                      ++ PD.sections spec
                                }

    resolvePath path = do
      let combinedPath = case specPath of
            Nothing -> path
            Just specPath ->
              if isAbsolute path
                then path
                else combine (takeDirectory specPath) path
      exists <- doesPathExist combinedPath
      if exists
        then Just <$> canonicalizePath combinedPath
        else return Nothing

    importer = fromMaybe "STDIN" specPath

    updE path = \case
      GuardedBinding xs -> GuardedBinding $ map (upd path) xs
      PatternBinding x y -> PatternBinding (upd path x) (upd path y)
      SetBinding x -> SetBinding $ upd path x
      RangeBinding x g y h -> RangeBinding (upd path x) g (upd path y) h

    updB path Binding {..} =
      Binding
        { bIdent = bIdent,
          bArgs = map (second (\x -> x {srcPath = Just path})) bArgs,
          bPos = bPos {srcPath = Just path},
          bVal = updE path bVal
        }

    upd path e =
      applySub (upd path) e {srcPos = (srcPos e) {srcPath = Just path}}

-----------------------------------------------------------------------------

initiate ::
  (SectionType, Formula Int) -> Formula Int
initiate (t, fml) = case t of
  InitiallyAssume -> fml
  Assume n -> iterate Next fml !! n
  AlwaysAssume n -> iterate Next (Globally fml) !! n
  InitiallyGuarantee -> fml
  Guarantee n -> iterate Next fml !! n
  AlwaysGuarantee n -> iterate Next (Globally fml) !! n

-----------------------------------------------------------------------------

symtable ::
  RD.Specification -> SymbolTable
symtable RD.Specification {..} =
  let -- minimal element (taken from 'names' table)
      minkey = key IM.minViewWithKey
      -- maximal element (taken from 'names' table)
      maxkey = key IM.maxViewWithKey
      -- get a list of all top-level expressions in the specification
      es = concatMap (getExprs . bVal) definitions ++ map snd sections
      -- get the list of all outputs of the specification
      so = foldl extractOutputs empty es
      -- get the list of all inputs of the specification
      si = foldl (extractInputs scopes types) empty es
      -- maps every output to its dependendencies, i.e., the functions
      -- and inputs it can be computed from
      oa =
        IM.fromList $
          map (\x -> (fst $ head x, map snd x)) $
            groupBy ((==) `on` fst) $
              sortBy (compare `on` fst) $
                toList $
                  foldl extractOutputAssignments empty es

      idT i = {-updType si so i $ -} fromMaybe (TPoly i) $ IM.lookup i types

      -- list of identifiers sorted by dependencies
      is' =
        topSort $
          transposeG $
            buildG (minkey, maxkey) $
              concatMap (\(i, xs) -> map (i,) xs) $
                IM.toAscList dependencies

      -- update mapping accoording to dependency order
      uD = (IM.fromList $ zip is' [minkey, minkey + 1 .. maxkey])

      -- update array according to depencency order
      aD :: A.Array Int Int
      aD = A.array (minkey, maxkey) $ IM.toAscList uD

      -- depencency ordering
      cmpD :: Int -> Int -> Ordering
      cmpD x y = compare (aD A.! x) (aD A.! y)

      -- ordering accoring to expression class, using dependency
      -- ordering for equivalent classes
      cmp a b =
        let aT = tEn so si a $ idT a
            bT = tEn so si b $ idT b
         in if
                | aT /= bT -> compare aT bT
                | otherwise -> cmpD a b

      -- sorted identifiers by above ordering
      is = sortBy cmp $ IM.keys names
   in symbolTable $
        A.array (minkey, maxkey) $
          map (\i -> (i, entry oa si so i)) is
  where
    getExprs = \case
      GuardedBinding xs -> xs
      PatternBinding x y -> [x, y]
      SetBinding x -> [x]
      RangeBinding x _ y _ -> [x, y]

    entry oa si so i =
      let t = fromMaybe (TPoly i) $ IM.lookup i types
          as = fromMaybe [] $ IM.lookup i arguments
          ds = fromMaybe [] $ IM.lookup i dependencies
       in IdRec
            { idName = assert (IM.member i names) (names IM.! i),
              idPos =
                assert (IM.member i positions) $
                  Just (positions IM.! i),
              idArgs = as,
              idBindings =
                assert (IM.member i bindings) $
                  Just (bindings IM.! i),
              idType = t,
              idDeps =
                if
                    | member i so -> case IM.lookup i oa of
                        Just xs -> i : xs
                        Nothing -> ds
                    | otherwise -> ds,
              idKind = case IM.lookup i scopes of
                Just () -> Internal
                Nothing
                  | member i so -> Output
                  | member i si -> Input
                  | predicate t -> Predicate
                  | otherwise -> case t of
                      TSignal {} -> Constant
                      _ -> Function
            }

    predicate = \case
      TSignal TBoolean -> True
      TFml _ x -> predicate x
      _ -> False

    tEn ::
      Set Int -> Set Int -> Int -> ExprType -> Int

    tEn so si i = \case
      TSignal {}
        | member i si -> 0
        | member i so -> 1
        | otherwise -> 2
      TFml _ (TSignal TBoolean) -> 3
      TFml {} -> 4
      _ -> 5

    key f
      | IM.null names = 0
      | otherwise =
          fst $ fst $ fromJust $ f names

-----------------------------------------------------------------------------

extractInputs ::
  IM.IntMap () -> IM.IntMap ExprType -> Set Int -> Expr Int -> Set Int
extractInputs sc tt a e@Expr {..} = case expr of
  BaseId i -> case assert (IM.member i tt) (tt IM.! i) of
    TSignal {} -> case IM.lookup i sc of
      Nothing -> insert i a
      Just () -> a
    _ -> a
  _ -> foldl (extractInputs sc tt) a $ subExpressions e

-----------------------------------------------------------------------------

extractOutputs ::
  Set Int -> Expr Int -> Set Int
extractOutputs a e@Expr {..} = case expr of
  BaseUpd _ x -> insert x a
  _ -> foldl extractOutputs a $ subExpressions e

-----------------------------------------------------------------------------

extractOutputAssignments ::
  Set (Int, Int) -> Expr Int -> Set (Int, Int)
extractOutputAssignments a e@Expr {..} = case expr of
  BaseUpd x i -> collect i a x
  _ -> foldl extractOutputAssignments a $ subExpressions e
  where
    collect i a e@Expr {..} = case expr of
      BaseFn x y -> collect i (collect i a x) y
      BaseConFn j -> insert (i, j) a
      BaseId j -> insert (i, j) a
      _ -> foldl (collect i) a $ subExpressions e

-----------------------------------------------------------------------------

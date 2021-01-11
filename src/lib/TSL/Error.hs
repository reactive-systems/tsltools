-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Error
-- Maintainer  :  Felix Klein
--
-- Data structures to wrap all contents, that are needed to print nice
-- error messages.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

-----------------------------------------------------------------------------

module TSL.Error
  ( Error
  , syntaxError
  , runtimeError
  , typeError
  , bindingError
  , conversionError
  , depError
  , cfgError
  , parseError
  , genericError
  , prError
  , prErrPos
  , errUnknown
  , errConflict
  , errPattern
  , errConditional
  , errCircularImp
  , errCircularDep
  , errExpect
  , errRange
  , errFormat
  ) where

-----------------------------------------------------------------------------

import TSL.Types (ExprType(..), prType, reducer)

import TSL.Expression (ExprPos(..), SrcPos(..))

import Text.Parsec.Error (ParseError)

import System.Exit (exitFailure)

import System.IO (hPrint, stderr)

import Control.Monad.State (StateT(..))

import Data.Maybe (fromMaybe)

import Data.IntMap (empty, insert)

import qualified Data.IntMap as IM (lookup)

-----------------------------------------------------------------------------

data Error
  = ErrType TypeError
  | ErrParse ParseError
  | ErrBnd BindingError
  | ErrDep DependencyError
  | ErrSyntax SyntaxError
  | ErrRunT RunTimeError
  | ErrConv ConvError
  | ErrCfg CfgError
  | ErrFormat FormatError
  | ErrGeneric GenericError

-----------------------------------------------------------------------------

newtype GenericError =
  GenericError
    { errGen :: String
    }

-----------------------------------------------------------------------------

newtype FormatError =
  FormatError
    { errFmt :: String
    }

-----------------------------------------------------------------------------

data TypeError =
  TypeError
    { errTPos :: ExprPos
    , errTMsgs :: [String]
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

data BindingError =
  BindingError
    { errBPos :: ExprPos
    , errBMsgs :: [String]
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

data DependencyError =
  DependencyError
    { errDPos :: ExprPos
    , errDMsgs :: [String]
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

data SyntaxError =
  SyntaxError
    { errSPos :: ExprPos
    , errSMsgs :: [String]
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

data RunTimeError =
  RunTimeError
    { errRPos :: ExprPos
    , errRMsgs :: [String]
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

data ConvError =
  ConvError
    { title :: String
    , cmsg :: String
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

newtype CfgError =
  ConfigError
    { fmsg :: String
    }
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

instance Show Error where
  show = \case
    ErrParse x                   -> show x
    ErrType TypeError {..}       -> pr "Type Error" errTPos errTMsgs
    ErrBnd BindingError {..}     -> pr "Binding Error" errBPos errBMsgs
    ErrDep DependencyError {..}  -> pr "Dependency Error" errDPos errDMsgs
    ErrSyntax SyntaxError {..}   -> pr "Syntax Error" errSPos errSMsgs
    ErrRunT RunTimeError {..}    -> pr "Evaluation Error" errRPos errRMsgs
    ErrCfg ConfigError {..}      -> "\"Error\":\n" ++ fmsg
    ErrConv ConvError {..}       ->
      "\"Conversion Error\": " ++ title ++ "\n" ++ cmsg
    ErrFormat FormatError {..}   ->
      "\"Format Error\": Unexpected format" ++ "\n" ++ errFmt
    ErrGeneric GenericError {..} -> "Error: " ++ errGen

    where
      pr errname pos msgs =
        "\"" ++ errname ++ "\" (" ++ prErrPos pos ++ "):\n" ++ concat msgs

-----------------------------------------------------------------------------

-- | Use this error constructor, if some sytax related misbehavior is
-- detected.

syntaxError
  :: ExprPos -> String -> Either Error a

syntaxError pos =
  Left . ErrSyntax . SyntaxError pos . return

-----------------------------------------------------------------------------

-- | Use this error constructor, if some runtime execution fails.

runtimeError
  :: ExprPos -> String -> Either Error a

runtimeError pos =
  Left . ErrRunT . RunTimeError pos . return

-----------------------------------------------------------------------------

-- | Use this error constructor, if some type related misbehavior is
-- detected.

typeError
  :: ExprPos -> String -> Either Error a

typeError pos =
  Left . ErrType . TypeError pos . return

-----------------------------------------------------------------------------

-- | Use this error constructor, if some identifier binding related
-- misbehavior is detected.

bindingError
  :: ExprPos -> String -> Either Error a

bindingError pos =
  Left . ErrBnd . BindingError pos . return

-----------------------------------------------------------------------------

-- | Use this error constructor, if some misbehavior concerning dependencies
-- between identifiers is detected.

depError
  :: ExprPos -> String -> Either Error a

depError pos =
  Left . ErrDep . DependencyError pos . return

-----------------------------------------------------------------------------

-- | Use this error constructor, if some unresolvable inconsistency in the
-- configuration exists.

cfgError
  :: String -> Either Error a

cfgError =
  Left . ErrCfg . ConfigError

-----------------------------------------------------------------------------

-- | Use this error constructor, if an invalid command line setting is
-- detected.

conversionError
  :: String -> String -> Either Error a

conversionError t =
  Left . ErrConv . ConvError t

-----------------------------------------------------------------------------

-- | Use this error constructor, whenever a parser fails.

parseError
  :: ParseError -> Either Error a

parseError =
  Left . ErrParse

-----------------------------------------------------------------------------

-- | Use this error, whenever something failed internally

genericError
  :: String -> Either Error a

genericError =
  Left . ErrGeneric . GenericError

-----------------------------------------------------------------------------

-- | Prints an error to STDERR and then terminates the program.

prError
  :: Error -> IO a

prError err =
  hPrint stderr (show err) >> exitFailure

-----------------------------------------------------------------------------

-- | Prints the position of an error related token.

prErrPos
  :: ExprPos -> String

prErrPos pos =
  let
    bl = srcLine $ srcBegin pos
    bc = srcColumn $ srcBegin pos
    el = srcLine $ srcEnd pos
    ec = srcColumn $ srcEnd pos
  in
    concat
      [ "line "
      , show bl
      , ", "
      , "column "
      , show bc
      , if bl == el
        then " - " ++ show ec
        else " - line " ++ show el ++ ", column " ++ show ec
      , case srcPath pos of
          Nothing   -> ""
          Just path -> ", " ++ path
      ]

-----------------------------------------------------------------------------

-- | Throws an error that indicates an unbound identifier name.

errUnknown
  :: String -> ExprPos -> StateT a (Either Error) b

errUnknown i pos =
  let msg = "identifiyer not in scope: " ++ i
  in StateT $ const $ bindingError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates two conflicting identifier bindings.

errConflict
  :: String -> ExprPos -> ExprPos -> StateT a (Either Error) b

errConflict i x y =
  let
    msg =
      "conflicting definitions: " ++
      i ++ "\n" ++ "already bound at " ++ prErrPos x
  in
    StateT $ const $ bindingError y msg

-----------------------------------------------------------------------------

-- | Throws an error informing the user that formulas cannot be used
-- as a right hand side of a pattern matching.

errPattern
  :: ExprPos -> StateT a (Either Error) b

errPattern pos =
  let
    msg =
      "Formulas are not allowed on the right hand side " ++
      "of a pattern match."
  in
    StateT $ const $ typeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a sub-expression that does not conform
-- to the big-operator notation.

errConditional
  :: ExprPos -> StateT a (Either Error) b

errConditional pos =
  let
    msg =
      "expecting expression of the form:\n" ++
      "  identifyer <- set"
  in
    StateT $ const $ syntaxError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a set of identifiers that decribe a
-- circular dependency between each other.

errCircularDep
  :: [(String, ExprPos)] -> ExprPos -> Either Error b

errCircularDep xs pos =
  let
    m = foldl max (length $ fst $ head xs) $ map (length . fst) xs
    msg =
      "detected circular dependencies between:" ++
      concatMap
        (\(x, y) ->
           "\n  " ++
           x ++
           replicate (m - length x) ' ' ++
           " (defined at " ++ prErrPos y ++ ")")
        xs ++
        if length xs > 1
          then ""
          else " depends on itself"
   in
     depError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates a set of imports that decribe a
-- circular dependency between each other.

errCircularImp
  :: [(String, ExprPos)] -> ExprPos -> Either Error b

errCircularImp xs pos =
  let
    m = foldl max (length $ fst $ head xs) $ map (length . fst) xs
    msg =
      "detected circular imports:" ++
      concatMap
        (\(x, y) ->
           "\n  " ++
           x ++
           replicate (m - length x) ' ' ++
           " (imported at " ++ prErrPos y ++ ")")
        xs
   in
     depError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that incicates a wrongly typed subexpression.

errExpect
  :: ExprType -> ExprType -> ExprPos -> StateT a (Either Error) b

errExpect x y pos =
  let
    f i = fromMaybe (TPoly i) $ IM.lookup i $ joinPoly empty x y
    r = reducer [upd f x, upd f y]
    msg =
      "expecting expression of type: " ++
      prType r (upd f x) ++
      "\n" ++ "but found expression of type: " ++ prType r (upd f y)
   in
     StateT $ const $ typeError pos msg

  where
    upd f = \case
      TSignal t -> TSignal $ upd f t
      TFml t t' -> TFml (upd f t) $ upd f t'
      TPoly i   -> f i
      TSet t    -> TSet $ upd f t
      t         -> t

    readType im = \case
      TSignal t -> readType im t
      TPoly i   -> case IM.lookup i im of
        Nothing -> Left i
        Just t  -> case t of
          TSignal (TPoly j)
            | i == j    -> Left i
            | otherwise -> readType im $ TPoly j
          TPoly j
            | i == j    -> Left i
            | otherwise -> readType im $ TPoly j
          _ -> Right t
      t -> Right t

    joinPoly im (TFml x y)       (TFml x' y')     = joinPoly (joinPoly im x x') y y'
    joinPoly im TFml {}          _                = im
    joinPoly im _                TFml {}          = im
    joinPoly im (rmS -> TPoly i) (rmS -> TPoly j) =
      case (readType im (TPoly i), readType im (TPoly j)) of
        (Left i', Left j') ->
          let t = TPoly $ min i' j'
          in insert i t $ insert j t $ insert i' t $ insert j' t im
        (Left i', Right t) ->
          insert i t $ insert i' t $ insert j t im
        (Right t, Left j') ->
          insert i t $ insert j' t $ insert j t im
        _ -> im
    joinPoly im (rmS -> TPoly i) t                 =
      case readType im (TPoly i) of
        Left i' -> insert i t $ insert i' t im
        _       -> im
    joinPoly im t               (rmS -> TPoly i)   =
      case readType im (TPoly i) of
        Left i' -> insert i t $ insert i' t im
        _       -> im
    joinPoly im _               _                 = im

    rmS =
      \case
        TSignal x -> rmS x
        x         -> x

-----------------------------------------------------------------------------

-- | Throws an error that incicates a sub-expression that does not conform
-- to the range syntax.

errRange
  :: ExprType -> ExprPos -> StateT a (Either Error) b

errRange x pos =
  let
    msg =
      "expecting: range expression\n" ++
      "but found: " ++ prType (reducer [x]) x ++ " expression"
  in
    StateT $ \_ -> typeError pos msg

-----------------------------------------------------------------------------

-- | Throws an error that indicates the occurence of an unexpected
-- format.

errFormat
  :: String -> Either Error a

errFormat =
  Left . ErrFormat . FormatError

-----------------------------------------------------------------------------

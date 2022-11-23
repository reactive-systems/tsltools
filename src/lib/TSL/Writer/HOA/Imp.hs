{-# LANGUAGE PatternSynonyms #-}
{- Template code writer for imperative languages -}
{-# LANGUAGE RecordWildCards #-}

module TSL.Writer.HOA.Imp where

import Control.Monad (zipWithM)
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Reader
import Data.Char (isSpace)
import Data.List
  ( intercalate,
    isPrefixOf,
  )
import qualified Hanoi as H
import qualified TSL.Writer.HOA.Codegen as CG

data ImpConfig = ImpConfig
  { -- binary functions
    impAdd :: String,
    impSub :: String,
    impMult :: String,
    impDiv :: String,
    -- binary comparators
    impEq :: String,
    impNeq :: String,
    impLt :: String,
    impGt :: String,
    impLte :: String,
    impGte :: String,
    -- logic
    impAnd :: String,
    impTrue :: String,
    impFalse :: String,
    impNot :: String,
    -- language constructs
    impIf :: String,
    impElif :: String,
    impCondition :: String -> String,
    impFuncApp :: String -> [String] -> String,
    impAssign :: String -> String -> String,
    impIndent :: Int -> String,
    impBlockStart :: String,
    impBlockEnd :: String
  }

withConfig :: ImpConfig -> H.HOA -> String
withConfig config hoa =
  let prog = CG.codegen hoa in Reader.runReader (writeProgram prog) config

-- | WRITE PROGRAM TO STRING
type Imp a = Reader ImpConfig a

writeProgram :: CG.Program -> Imp String
writeProgram (CG.Program stateTransList) = do
  lines <-
    concat <$> zipWithM writeStateTrans (False : repeat True) stateTransList
  return $ intercalate "\n" $ discardEmptyLines lines
  where
    discardEmptyLines lines =
      filter (\l -> not (null l) && not (all isSpace l)) lines

writeStateTrans :: Bool -> CG.StateTrans -> Imp [String]
writeStateTrans useElif (CG.StateTrans state transList) = do
  ImpConfig {..} <- Reader.ask
  let opIf = if useElif then impElif else impIf

  innerLines <- concat <$> zipWithM writeTrans (False : repeat True) transList

  return $
    [ opIf
        ++ " "
        ++ impCondition ("currentState " ++ impEq ++ " " ++ state)
        ++ impBlockStart
    ]
      ++ map (impIndent 1 ++) innerLines
      ++ [impBlockEnd]

writeTrans :: Bool -> CG.Trans -> Imp [String]
writeTrans useElif (CG.Trans ps us target) = do
  ImpConfig {..} <- Reader.ask
  let opIf = if useElif then impElif else impIf

  ps' <- mapM writePredicate ps
  us' <- mapM writeUpdate us
  let condition = intercalate (" " ++ impAnd ++ " ") ps'
      assignments = map (impIndent 1 ++) us'

  return $
    [opIf ++ " " ++ impCondition condition ++ impBlockStart]
      ++ assignments
      ++ [impIndent 1 ++ impAssign "currentState" target]
      ++ [impBlockEnd]

writePredicate :: CG.Predicate -> Imp String
writePredicate p = do
  ImpConfig {..} <- Reader.ask
  case p of
    CG.PTrue -> return impTrue
    CG.PFalse -> return impFalse
    CG.PNot (CG.PNot p') -> writePredicate p'
    CG.PNot CG.PTrue -> return impFalse
    CG.PNot CG.PFalse -> return impTrue
    CG.PNot (CG.PTerm (Eq x1 x2)) -> writeTerm (Neq x1 x2)
    CG.PNot (CG.PTerm (Neq x1 x2)) -> writeTerm (Eq x1 x2)
    CG.PNot (CG.PTerm (Lt x1 x2)) -> writeTerm (Gte x1 x2)
    CG.PNot (CG.PTerm (Gt x1 x2)) -> writeTerm (Lte x1 x2)
    CG.PNot (CG.PTerm (Lte x1 x2)) -> writeTerm (Gt x1 x2)
    CG.PNot (CG.PTerm (Gte x1 x2)) -> writeTerm (Lt x1 x2)
    CG.PNot p' -> (impNot ++) <$> writePredicate p'
    CG.PTerm term -> writeTerm term

writeUpdate :: CG.Update -> Imp String
writeUpdate (CG.Update var term) = do
  ImpConfig {..} <- Reader.ask
  impAssign var <$> writeTermNoParens term

writeTerm :: CG.Term -> Imp String
writeTerm term = do
  ImpConfig {..} <- Reader.ask
  case term of
    CG.Var x -> return x
    CG.App f args ->
      if isTSLMTBinOp f args
        then do
          bin <- writeTermApp f args
          return $ "(" ++ bin ++ ")"
        else writeTermNoParens term

writeTermNoParens :: CG.Term -> Imp String
writeTermNoParens term = do
  ImpConfig {..} <- Reader.ask
  case term of
    CG.Var x -> return x
    CG.App f args -> writeTermApp f args

writeTermApp :: String -> [CG.Term] -> Imp String
writeTermApp f args
  | isTSLMTLiteral f args = return $ replaceTSLMTLiteral f
  | isTSLMTBinOp f args = do
      args' <- mapM writeTerm args
      let [x1, x2] = args'
      replaceTSLMTBinOp f x1 x2
  | otherwise = do
      ImpConfig {..} <- Reader.ask
      impFuncApp f <$> mapM writeTermNoParens args

-- | HELPERS
pickIfOrElif :: Bool -> Imp String
pickIfOrElif useElif = do
  if useElif then Reader.asks impElif else Reader.asks impIf

isTSLMTLiteral :: String -> [CG.Term] -> Bool
isTSLMTLiteral s args = null args && (isReal s || isInt s)

replaceTSLMTLiteral :: String -> String
replaceTSLMTLiteral s
  | isReal s = replaceNeg $ drop 4 s
  | isInt s = replaceNeg $ drop 3 s
  | otherwise = s
  where
    replaceNeg x = if "Neg" `isPrefixOf` x then "-" ++ drop 3 x else x

isReal :: String -> Bool
isReal s = "real" `isPrefixOf` s

isInt :: String -> Bool
isInt s = "int" `isPrefixOf` s

isTSLMTBinOp :: String -> [CG.Term] -> Bool
isTSLMTBinOp f args = length args == 2 && isOp
  where
    isOp = case f of
      "add" -> True
      "sub" -> True
      "mult" -> True
      "div" -> True
      "eq" -> True
      "neq" -> True
      "lt" -> True
      "gt" -> True
      "lte" -> True
      "gte" -> True
      _ -> False

replaceTSLMTBinOp :: String -> String -> String -> Imp String
replaceTSLMTBinOp f x1 x2 = do
  ImpConfig {..} <- Reader.ask
  return $ case f of
    "add" -> useBinOp impAdd
    "sub" -> useBinOp impSub
    "mult" -> useBinOp impMult
    "div" -> useBinOp impDiv
    "eq" -> useBinOp impEq
    "neq" -> useBinOp impNeq
    "lt" -> useBinOp impLt
    "gt" -> useBinOp impGt
    "lte" -> useBinOp impLte
    "gte" -> useBinOp impGte
    x ->
      error ("Implementation bug: " ++ x ++ " is not a TSLMT binary operation.")
  where
    useBinOp op = x1 ++ " " ++ op ++ " " ++ x2

pattern Add :: CG.Term -> CG.Term -> CG.Term
pattern Add x1 x2 = CG.App "add" [x1, x2]

pattern Sub :: CG.Term -> CG.Term -> CG.Term
pattern Sub x1 x2 = CG.App "sub" [x1, x2]

pattern Mult :: CG.Term -> CG.Term -> CG.Term
pattern Mult x1 x2 = CG.App "mult" [x1, x2]

pattern Div :: CG.Term -> CG.Term -> CG.Term
pattern Div x1 x2 = CG.App "div" [x1, x2]

pattern Eq :: CG.Term -> CG.Term -> CG.Term
pattern Eq x1 x2 = CG.App "eq" [x1, x2]

pattern Neq :: CG.Term -> CG.Term -> CG.Term
pattern Neq x1 x2 = CG.App "neq" [x1, x2]

pattern Lt :: CG.Term -> CG.Term -> CG.Term
pattern Lt x1 x2 = CG.App "lt" [x1, x2]

pattern Gt :: CG.Term -> CG.Term -> CG.Term
pattern Gt x1 x2 = CG.App "gt" [x1, x2]

pattern Lte :: CG.Term -> CG.Term -> CG.Term
pattern Lte x1 x2 = CG.App "lte" [x1, x2]

pattern Gte :: CG.Term -> CG.Term -> CG.Term
pattern Gte x1 x2 = CG.App "gte" [x1, x2]

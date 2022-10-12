------------------------------------------------------------------------------
	-- |
-- Module      :  TSL.ModuloTheories.Preprocessor
-- Description :  
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------

module TSL.Preprocessor(preprocess) where

-------------------------------------------------------------------------------

import Control.Monad (liftM2)

import Control.Applicative ((<|>))

import Text.Parsec (ParseError, endBy, sepBy, try, choice, eof)

import Text.Parsec.Combinator (chainl1)

import Text.Parsec.String (Parser)

import TSL.Error (Error, parseError)

import qualified Data.Char as Char

import qualified Text.Parsec as Parsec

-------------------------------------------------------------------------------

data Op = 
      Eq
    | Lt
    | Gt
    | Lte
    | Gte
    | Add
    | Sub
    | Mult
    | Div

instance Show Op where
  show = \case
    Eq   -> "eq"
    Lt   -> "lt"
    Gt   -> "gt"
    Lte  -> "lte"
    Gte  -> "gte"
    Add  -> "add"
    Sub  -> "sub"
    Mult -> "mult"
    Div  -> "div"

ltOp :: Parser Op
ltOp = do
    Parsec.string "<"
    Parsec.notFollowedBy $ Parsec.oneOf "-="
    return Lt

lteOp :: Parser Op
lteOp = do
    Parsec.string "<="
    return Lte

gtOp :: Parser Op
gtOp = do
    Parsec.string ">"
    Parsec.notFollowedBy $ Parsec.char '='
    return Gt

gteOp :: Parser Op
gteOp = do
    Parsec.string "<="
    return Gte

eqOp :: Parser Op
eqOp = do
    Parsec.string "="
    return Eq

addOp :: Parser Op
addOp = do
    Parsec.string "+"
    return Add

subOp :: Parser Op
subOp = do
    Parsec.string "-"
    return Sub

multOp :: Parser Op
multOp = do
    Parsec.string "*"
    return Mult

divOp :: Parser Op
divOp = do
    Parsec.string "/"
    return Mult

comparator :: Parser Op
comparator = ltOp <|> gtOp <|> lteOp <|> gteOp <|> eqOp

arithmetic :: Parser Op
arithmetic = addOp <|> subOp <|> multOp <|> divOp

binOp :: Parser Op
binOp = comparator <|> arithmetic

-------------------------------------------------------------------------------

data Value =
      TSLInt  Int
    | TSLReal Float
    | Symbol String
    | SymbolList [Value]
    | BinOp Op Value Value
    | Paren Value

instance Show Value where
  show = \case
    TSLInt i         -> "int"  ++ show i ++ "()"
    TSLReal r        -> "real" ++ show r ++ "()"
    Symbol s         -> s
    SymbolList slist -> unwords $ map show slist
    BinOp op lhs rhs -> unwords $ [show op, show lhs, show rhs]
    Paren v          -> "(" ++ show v ++ ")"

int :: Parser Value
int = do
    int <- Parsec.many1 Parsec.digit
    return $ TSLInt $ read int

real :: Parser Value
real = do
    beforePoint <- Parsec.many1 Parsec.digit
    Parsec.char '.'
    afterPoint <- Parsec.many1 Parsec.digit
    return $ TSLReal $ read $ beforePoint ++ ('.':afterPoint)

binOpTerm :: Parser (Value -> Value -> Value)
binOpTerm = do
    maybeSpaces
    op <- binOp
    maybeSpaces
    return $ BinOp op

parens :: Parser Value
parens = do
  Parsec.string "("
  maybeSpaces
  val <- tslSequence
  maybeSpaces
  Parsec.string ")"
  return $ Paren val

symbol :: Parser Value
symbol = do
    str <- Parsec.many1 $ Parsec.noneOf "() \n\r"
    return $ Symbol str

atom :: Parser Value
atom = try real <|> int <|> symbol

factor :: Parser Value
factor = parens <|> atom

maybeSpaces :: Parsec.Stream s m Char => Parsec.ParsecT s u m [Char]
maybeSpaces = Parsec.many $ Parsec.char ' '

tslValue :: Parser Value
tslValue = chainl1 (factor <* maybeSpaces) (try binOpTerm)

tslSequence :: Parser Value
tslSequence = SymbolList <$> (tslValue >>= recursion)
  where recursion this = do { maybeSpaces
                            ; next <- tslValue
                            ; rest <- recursion next
                            ; return $ this:rest
                            }
                           <|> return [this]

lineParser :: Parser [Value]
lineParser = maybeSpaces *> sepBy tslSequence maybeSpaces
    
fileParser :: Parser [[Value]]
fileParser = endBy lineParser Parsec.endOfLine

parse :: String -> Either ParseError [[Value]]
parse input = Parsec.parse (fileParser <* eof) err input
  where err = "Parser Failed!:\n>>>>>\n" ++ input ++ "\n<<<<<\n"

preprocess :: String -> Either Error String
preprocess s = case parse s of
  Right values -> return $ unlines $ map (unwords . map show) values
  Left err     -> parseError err

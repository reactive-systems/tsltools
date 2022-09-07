-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Parser.Preprocessor
-- Description :  
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Preprocessor () where

-------------------------------------------------------------------------------

import Control.Applicative ((<|>))

import Text.Parsec (ParseError, endBy, sepBy, try, choice, eof)

import Text.Parsec.Combinator (chainl1)

import Text.Parsec.String (Parser)

import qualified Data.Char as Char

import qualified Text.Parsec as Parsec

import Debug.Trace (trace)

-------------------------------------------------------------------------------

sampleLIA :: String
sampleLIA = unlines 
  [ "#LIA"
  , "always guarantee {"
  , "< 1 2;"
  , "}"
  ]

sampleReal :: String
sampleReal = unlines 
  [ "#RA"
  , "always guarantee {"
  , "[x <- 3.14 + 2.72];"
  , "}"
  ]

sampleEq :: String
sampleEq = unlines 
  [ "#EUF"
  , "always guarantee {"
  , "= x y;"
  , "}"
  ]

-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------

data Value =
      TSLInt  Int
    | TSLReal Float
    | Symbol String
    | BinOp Op Value Value
    | Paren Value

instance Show Value where
  show = \case
    TSLInt i         -> "int"  ++ show i ++ "()"
    TSLReal r        -> "real" ++ show r ++ "()"
    Symbol s         -> s
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

binOpTerm :: Parser Value
binOpTerm = do
    lhs <- tslValue
    Parsec.spaces
    op <- binOp
    Parsec.spaces
    rhs <- tslValue
    return $ BinOp op lhs rhs

parens :: Parser Value
parens = Parsec.between lpar rpar $ tslValue
  where lpar = Parsec.string "("
        rpar = Parsec.string ")"

-- Can we safely ignore && and || ?
symbol :: Parser Value
symbol = do
    str <- Parsec.many1 $ Parsec.satisfy $ not . Char.isSpace
    return $ Symbol str

primitive :: Parser Value
primitive = try real <|> int <|> symbol

onlySpaces :: Parsec.Stream s m Char => Parsec.ParsecT s u m [Char]
onlySpaces = Parsec.many $ Parsec.char ' '

tslValue :: Parser Value
tslValue = chainl1 (primitive <* onlySpaces) (try (BinOp <$> binOp <* onlySpaces))

lineParser :: Parser [Value]
lineParser = sepBy tslValue $ Parsec.many $ Parsec.char ' '
    
fileParser :: Parser [[Value]]
fileParser = endBy lineParser Parsec.endOfLine

parse :: String -> Either ParseError [[Value]]
parse = Parsec.parse (fileParser <* eof) "Parser Failed!"

spec :: [Char]
spec = sampleReal

main :: IO ()
main = do
    putStrLn spec
    case parse spec of
      Right res -> putStrLn $ unlines $ map (unwords . map show) res
      Left e -> error $ show e

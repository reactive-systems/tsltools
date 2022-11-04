{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------

-- |
-- Module      :  TSL.ModuloTheories.Preprocessor
-- Description :
-- Maintainer  :  Wonhyuk Choi
module TSL.Preprocessor
  ( preprocess
  ) where

-------------------------------------------------------------------------------

import           Control.Applicative            ( (<|>) )
import           TSL.Error                      ( Error
                                                , parseError
                                                )
import           Text.Parsec                    ( ParseError
                                                , endBy
                                                , eof
                                                , sepBy
                                                , try
                                                )
import qualified Text.Parsec                   as Parsec
import           Text.Parsec.Combinator         ( chainl1 )
import           Text.Parsec.String             ( Parser )

-------------------------------------------------------------------------------

data Op
  = Eq
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
  _ <- Parsec.string "<"
  _ <- Parsec.notFollowedBy $ Parsec.oneOf "-="
  return Lt

lteOp :: Parser Op
lteOp = do
  _ <- Parsec.string "<="
  return Lte

gtOp :: Parser Op
gtOp = do
  _ <- Parsec.string ">"
  _ <- Parsec.notFollowedBy $ Parsec.char '='
  return Gt

gteOp :: Parser Op
gteOp = do
  _ <- Parsec.string "<="
  return Gte

eqOp :: Parser Op
eqOp = do
  _ <- Parsec.string "="
  return Eq

addOp :: Parser Op
addOp = do
  _ <- Parsec.string "+"
  return Add

subOp :: Parser Op
subOp = do
  _ <- Parsec.string "-"
  _ <- Parsec.notFollowedBy $ Parsec.char '>'
  return Sub

multOp :: Parser Op
multOp = do
  _ <- Parsec.string "*"
  return Mult

divOp :: Parser Op
divOp = do
  _ <- Parsec.string "/"
  return Div

comparator :: Parser Op
comparator = ltOp <|> gtOp <|> lteOp <|> gteOp <|> eqOp

arithmetic :: Parser Op
arithmetic = addOp <|> subOp <|> multOp <|> divOp

binOp :: Parser Op
binOp = comparator <|> arithmetic

-------------------------------------------------------------------------------

data Value
  = TSLInt String
  | TSLReal String
  | Symbol String
  | SymbolList [Value]
  | BinOp Op Value Value
  | Paren Value

instance Show Value where
  show = \case
    TSLInt     s     -> "int" ++ s ++ "()"
    TSLReal    s     -> "real" ++ s ++ "()"
    Symbol     s     -> s
    SymbolList slist -> unwords $ map show slist
    BinOp op lhs rhs -> unwords [show op, show lhs, show rhs]
    Paren v          -> "(" ++ show v ++ ")"

int :: Parser Value
int = do
  s <- Parsec.many1 Parsec.digit
  return $ TSLInt s

real :: Parser Value
real = do
  beforePoint <- Parsec.many1 Parsec.digit
  _           <- Parsec.char '.'
  afterPoint  <- Parsec.many1 Parsec.digit
  return $ TSLReal $ beforePoint ++ ('.' : afterPoint)

binOpTerm :: Parser (Value -> Value -> Value)
binOpTerm = do
  _  <- maybeSpaces
  op <- binOp
  _  <- maybeSpaces
  return $ BinOp op

parens :: Parser Value
parens = do
  _   <- Parsec.string "("
  _   <- maybeSpaces
  val <- tslSequence
  _   <- maybeSpaces
  _   <- Parsec.string ")"
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
 where
  recursion this =
    do
      _    <- maybeSpaces
      next <- tslValue
      rest <- recursion next
      return $ this : rest
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
  Left  err    -> parseError err

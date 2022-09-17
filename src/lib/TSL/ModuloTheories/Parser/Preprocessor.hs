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

-------------------------------------------------------------------------------

import Control.Monad (liftM2)

import Control.Applicative ((<|>))

import Text.Parsec (ParseError, endBy, sepBy, try, choice, eof)

import Text.Parsec.Combinator (chainl1)

import Text.Parsec.String (Parser)

import qualified Data.Char as Char

import qualified Text.Parsec as Parsec

import Debug.Trace (trace)

-------------------------------------------------------------------------------

debug :: (Monad m, Show a) => m a -> m a
debug = (liftM2 trace show return =<<)

sampleLIA :: String
sampleLIA = unlines 
  [ "#LIA"
  , "always guarantee {"
  , "1 < 2;"
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
  , "x = y;"
  , "}"
  ]

sampleParen :: String
sampleParen = unlines 
  [ "#LIA"
  , "always guarantee {"
  , "(x + y) = (y + x) ;"
  , "}"
  ]

sampleLogic :: String
sampleLogic = unlines 
  [ "#EUF"
  , "always guarantee {"
  , "[m <- ((p u z && x) = (a y))] && ( ((f y) = (g z h) ) || (y = w));"
  , "}"
  ]

sampleTsl :: String
sampleTsl = unlines 
  [ "#UF"
  , "always guarantee {"
  , "[a <- b c d] || [e <- f g];"
  , " (h i j (k&& l) ) ;"
  , "}"
  ]

samples = [sampleEq, sampleLIA, sampleReal, sampleParen, sampleLogic, sampleTsl]
sample = sampleTsl

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
    | SymbolList [Value]
    | BinOp Op Value Value
    | Paren Value
  -- deriving (Show)

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

-- primitive :: Parser Value
-- primitive = SymbolList <$> (atom >>= recursion)
--   where recursion this = do { maybeSpaces -- try?
--                             ; next <- atom
--                             ; rest <- recursion next
--                             ; return $ this:rest
--                             }
--                            <|> return [this]

-- sequence :: Parser Value
-- can be of form 
-- h i j (k && (a + b) )
-- need to handle tslValues inside (i.e. parens).

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
parse = Parsec.parse (fileParser <* eof) "Parser Failed!"

eval :: String -> IO ()
eval spec = do
    putStrLn "\n---Originally--"
    putStrLn spec
    putStrLn "---After Parse--\n"
    case parse spec of
      Right res -> putStrLn $ unlines $ map (unwords . map show) res
      -- Right res -> mapM_ print res
      Left e -> error $ show e
    putStrLn "---End---\n"

main :: IO ()
main = mapM_ eval samples
-- main = eval sampleTsl

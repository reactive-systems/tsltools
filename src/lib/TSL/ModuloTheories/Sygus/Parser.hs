-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Parser
-- Description :  Parses SyGuS Solver result to internal data structures.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Parser
  ( parseSygusResult
  , parseModels
  ) where

-------------------------------------------------------------------------------

import Data.List (intersperse)

import Control.Applicative ((<|>))

import TSL.Error (Error, parseError, errModel)

import TSL.ModuloTheories.Sygus.Common (Expansion (..) , Term (..), Model(..))

-------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

import Text.Parsec.String (Parser)

import qualified Text.Parsec as Parsec


parens :: Parser a -> Parser a
parens = Parsec.between lpar rpar
  where lpar = Parsec.string "("
        rpar = Parsec.string ")"

nonReserved :: Parser Char
nonReserved = Parsec.noneOf " ()!"

expansionParser :: Parser (Expansion String)
expansionParser = parens $ expansion
  where expansion = do
          _    <- Parsec.string "!"
          rule <- Parsec.space >> termParser
          _    <- Parsec.space >> Parsec.string ":gterm"
          sink <- Parsec.space >> Parsec.many1 nonReserved
          return $ Expansion sink rule

valueParser :: Parser (Term String)
valueParser = Value <$> literalParser

exprParser :: Parser (Term String)
exprParser = Expression <$> expansionParser

funcParser :: Parser (Term String)
funcParser = parens fxn
  where fxn = do
         function <- Parsec.many1 nonReserved
         args     <- Parsec.space >> termParser `Parsec.sepBy` Parsec.space
         return $ Function function args

termParser :: Parser (Term String)
termParser = Parsec.choice [Parsec.try exprParser, Parsec.try funcParser, valueParser]

sygusParser :: Parser (Term String)
sygusParser = parens $ (header >> Parsec.space >> termParser)
    where header = Parsec.string "sygus-sol-gterm"

makeErrMsg :: String -> String
makeErrMsg input = header ++ input ++ footer
  where header = "Parser Failed!:\n>>>>>\nInput was: \""
        footer = "\"\n<<<<<\n"

parseSygusResult :: String -> Either Error (Term String)
parseSygusResult input =
  case Parsec.parse (sygusParser <* Parsec.eof) (makeErrMsg input) input of
    Left err   -> parseError err
    Right term -> return term

example :: String
example = unlines 
  [ "sat"
  , "("
  , "(define-fun y () Int (- 1))"
  , "(define-fun x () Int 0)"
  , ")"
  ]

nullary :: Parser ()
nullary = Parsec.string "()" >> return ()

negativeLiteralParser :: Parser String
negativeLiteralParser = parens innerVal
  where innerVal = do
          _       <- Parsec.string "-"
          _       <- Parsec.space
          literal <- literalParser
          return $ "(- " ++ literal ++ ")"

literalParser :: Parser String
literalParser = negativeLiteralParser <|> Parsec.many1 nonReserved

symbolType :: Parser ()
symbolType = literalParser >> return ()

modelParser :: Parser (Model String)
modelParser = parens modelParserInner
  where modelParserInner = do
          _      <- Parsec.string "define-fun"
          symbol <- Parsec.space >> literalParser
          _      <- Parsec.space >> nullary
          _      <- Parsec.space >> symbolType
          value  <- Parsec.space >> literalParser
          return $ Model (symbol, value)

allModelsParser :: Parser [Model String]
allModelsParser =
  parens $ Parsec.space >> modelParser `Parsec.endBy1` Parsec.space

parseModels :: String -> Either Error [Model String]
parseModels input = case head (lines input) of
  "unsat"   -> errModel $ "Got UNSAT when trying to parse model: " ++ input
  "sat"     ->
      let modelResult = concat $ intersperse " " $ tail $ lines input
          errMsg      = makeErrMsg modelResult
    in case Parsec.parse (allModelsParser <* Parsec.eof) errMsg modelResult of
         Left err     -> parseError err
         Right models -> return models
  otherwise -> errModel $ "Unexpected model result: " ++ otherwise

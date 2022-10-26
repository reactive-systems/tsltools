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
  ( parseSolution
  ) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Sygus.Common (Expansion (..) , Term (..))

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
valueParser = Value <$> Parsec.many1 nonReserved

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

solutionParser :: Parser (Term String)
solutionParser = parens $ (header >> Parsec.space >> termParser)
    where header = Parsec.string "sygus-sol-gterm"

parseSolution :: String -> Either Parsec.ParseError (Term String)
parseSolution input =
  Parsec.parse (solutionParser <* Parsec.eof) errMsg input
  where errMsg = "Parser Failed!:\n>>>>>\n" ++ input ++ "\n<<<<<\n"

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

import qualified Data.Char as Char

import qualified Text.Parsec as Parsec

import Debug.Trace (trace)

ex1 :: String
ex1 = "(sygus-sol-gterm (! input :gterm b))"

ex2 :: String
ex2 = "(sygus-sol-gterm (! (! x :gterm J) :gterm I))"

ex3 :: String
ex3 = "(sygus-sol-gterm (! (+ 1 (+ 1 1)) :gterm I))"

ex4 :: String
ex4 = "(sygus-sol-gterm (! (+ (! 1 :gterm a) (! 1 :gterm b)) :gterm a))"

examples :: [String]
examples = [ex1, ex2, ex3, ex4]

-- S         ::= (sygus-sol-gterm TERM)
-- EXPANSION ::= (! TERM :gterm a)
-- TERM      ::= Value | (Function [TERM]) | EXPANSION

type SygusTerm = Term String

parens :: Parser a -> Parser a
parens = Parsec.between lpar rpar
  where lpar = Parsec.string "("
        rpar = Parsec.string ")"

nonReserved :: Parser Char
nonReserved = Parsec.noneOf " ()!"

expansionParser :: Parser (Expansion String)
expansionParser = parens $ expansion
  where expansion = do
          trace "\n---> EXPANSION" $ return ()
          _    <- Parsec.string "!"
          rule <- Parsec.space >> termParser
          trace ("------>RULE:" ++ show rule) (return ())
          _    <- Parsec.space >> Parsec.string ":gterm"
          sink <- Parsec.space >> Parsec.many1 nonReserved
          trace ("------>SINK:" ++ show sink) (return ())
          return $ Expansion sink rule

valueParser :: Parser SygusTerm
-- valueParser = trace "\n---> VALUE\n" $ Value <$> Parsec.many1 nonReserved
valueParser = do
    trace "\n---> VALUE" $ return ()
    val <- Parsec.many1 nonReserved
    trace ("------>VALUE: " ++ show val) (return ())
    return $ Value val

exprParser :: Parser SygusTerm
-- exprParser = trace "\n---> EXPRESSION\n" $ Expression <$> expansionParser
exprParser = do
    trace "\n---> EXPRESSION" $ return ()
    expr <- expansionParser
    trace ("------>EXPR: " ++ show expr) (return ())
    return $ Expression expr

funcParser :: Parser SygusTerm
funcParser = trace "\n---> FUNCTION" $ parens fxn
  where fxn = do
         function <- Parsec.many1 nonReserved
         trace ("------>FUNCTION: " ++ show function) (return ())
         args <- Parsec.space >> termParser `Parsec.sepBy` Parsec.space
         trace ("------>ARGS: " ++ show args ++ " ") (return ())
         return $ Function function args

termParser :: Parser SygusTerm
termParser = Parsec.choice [Parsec.try exprParser, Parsec.try funcParser, valueParser]

solutionParser :: Parser SygusTerm
solutionParser = parens $ (header >> Parsec.space >> termParser)
    where header = Parsec.string "sygus-sol-gterm"

parseSolution :: String -> Either Parsec.ParseError (Term String)
parseSolution input =
  Parsec.parse (solutionParser <* Parsec.eof) errMsg input
  where errMsg = "Parser Failed!:\n>>>>>\n" ++ input ++ "\n<<<<<\n"

-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Parser.Global
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Parsers for global definitions and formula sections.
--
-----------------------------------------------------------------------------

module TSL.Parser.Global
  ( assignmentParser
  , sectionParser
  , elementsParser
  ) where

-----------------------------------------------------------------------------

import TSL.Binding
  ( Binding(..)
  , BoundExpr(..)
  )

import TSL.Expression
  ( Expr(..)
  , ExprPos(..)
  )

import TSL.Types
  ( SectionType(..)
  )

import TSL.Parser.Data
  ( globalDef
  )

import TSL.Parser.Utils
  ( identifier
  )

import TSL.Parser.Expression
  ( exprParser
  )

import Data.Functor.Identity
  ( Identity
  )

import Data.Maybe
  ( catMaybes
  )

import Control.Monad
  ( void
  )

import Text.Parsec
  ( (<|>)
  , oneOf
  , sepBy
  , many1
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec.Token
  ( GenLanguageDef(..)
  , GenTokenParser
  , reservedNames
  , whiteSpace
  , natural
  , makeTokenParser
  , reserved
  , braces
  , reservedOp
  )

-----------------------------------------------------------------------------

elementsParser
  :: Parser (Either (Binding String) (SectionType, [Expr String]))

elementsParser =
  (~~) >> (sectionParser <|> assignmentParser)

-----------------------------------------------------------------------------

-- | Parses a list of formulas within a section labeled by 'name'.

sectionParser
  :: Parser (Either a (SectionType, [Expr String]))

sectionParser = do
  t <- sectionTypeParser
  xs <- br $ sepBy (nonEmptyParser exprParser) $ rOp ";"
  return $ Right (t, catMaybes xs)

  where
    nonEmptyParser p =
      fmap return p <|> return Nothing

-----------------------------------------------------------------------------

sectionTypeParser
  :: Parser SectionType

sectionTypeParser =
      (keyword "initially"
       >> (    (keyword "assume" >> return InitiallyAssume)
           <|> (keyword "guarantee" >> return InitiallyGuarantee)
          )
      )
  <|> (keyword "always"
       >> (    (keyword "assume" >> afterParser AlwaysAssume)
           <|> (keyword "guarantee" >> afterParser AlwaysGuarantee)
          )
      )
  <|> (keyword "assume" >> afterParser Assume)
  <|> (keyword "guarantee" >> afterParser Guarantee)

  where
    afterParser c =
          fmap (c . fromIntegral) (keyword "after" >> natural tokenparser)
      <|> return (c 0)

-----------------------------------------------------------------------------

-- | Parses an assignment representing a single binding or a function
-- definition.

assignmentParser
  :: Parser (Either (Binding String) a)

assignmentParser = do
  (x,pos) <- identifier (~~)
  argumentsParser x pos
    <|> reminderParser x [] pos

  where
    argumentsParser x pos = do
      args <- many1 (identifier (~~))
      (~~)
      reminderParser x args $
        ExprPos (srcBegin pos) (srcEnd $ snd $ last args)

    reminderParser x args pos = do
      rOp "="
      es <- many1 exprParser
      rOp ";"
      return $ Left $
        Binding
          { bIdent = x
          , bArgs = args
          , bPos = pos
          , bVal = GuardedBinding es
          }

-----------------------------------------------------------------------------

tokenparser
  :: GenTokenParser String p Identity

tokenparser =
  makeTokenParser globalDef
    { opStart = oneOf "=;:"
    , opLetter = oneOf "=;:"
    , reservedOpNames = [ "=", ";", ":"]
    , reservedNames =
      [ "initially"
      , "always"
      , "assume"
      , "guarantee"
      , "after"
      ]
    }

-----------------------------------------------------------------------------

br
  :: Parser a -> Parser a

br = braces tokenparser

-----------------------------------------------------------------------------

rOp
  :: String -> Parser ()

rOp = reservedOp tokenparser

-----------------------------------------------------------------------------

(~~)
  :: Parser ()

(~~) = whiteSpace tokenparser

-----------------------------------------------------------------------------

keyword
  :: String -> Parser ()

keyword = void . reserved tokenparser

-----------------------------------------------------------------------------

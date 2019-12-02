-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Parser
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Parsing module containing all neccessary parsers.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    TupleSections

  #-}

-----------------------------------------------------------------------------

module TSL.Parser
  ( parse
  ) where

-----------------------------------------------------------------------------

import TSL.Error
  ( Error
  , parseError
  )

import TSL.Parser.Data
  ( Specification(..)
  )

import TSL.Parser.Global
  ( elementsParser
  )

import Data.Either
  ( partitionEithers
  )

import Text.Parsec
  ( many1
  )

import qualified Text.Parsec as P
  ( parse
  )

import Text.Parsec.String
  ( Parser
  )

-----------------------------------------------------------------------------

-- | @parseSpecification str @ parses a specification from the string
-- @str@.

parse
  :: String -> Either Error Specification

parse str =
  case P.parse specificationParser "Syntax Error" str of
    Left err -> parseError err
    Right x  -> return x

-----------------------------------------------------------------------------

specificationParser
  :: Parser Specification

specificationParser = do
  (xs,ys) <- partitionEithers <$> many1 elementsParser

  return Specification
    { definitions = xs
    , sections = concatMap (\(t,vs) -> map (t,) vs) ys
    }

-----------------------------------------------------------------------------

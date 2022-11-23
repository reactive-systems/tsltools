-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.Parser
-- Maintainer  :  Felix Klein
--
-- Parsing module containing all neccessary parsers.
module TSL.Parser
  ( parse,
  )
where

-----------------------------------------------------------------------------

import TSL.Error (Error, parseError)
import TSL.Parser.Data (Specification (..))
import TSL.Parser.Global (GlobalElement (..), elementsParser)
import Text.Parsec (many1)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)

-----------------------------------------------------------------------------

-- | @parseSpecification str @ parses a specification from the string
-- @str@.
parse ::
  String -> Either Error Specification
parse str =
  case P.parse specificationParser "Syntax Error" str of
    Left err -> parseError err
    Right x -> return x

-----------------------------------------------------------------------------

specificationParser ::
  Parser Specification
specificationParser = do
  (is, as, ss) <- partitionTypes ([], [], []) <$> many1 elementsParser

  return
    Specification
      { imports = is,
        definitions = as,
        sections = concatMap (\(t, vs) -> map (t,) vs) ss
      }
  where
    partitionTypes (is, as, ss) = \case
      [] -> (reverse is, reverse as, reverse ss)
      x : xr -> case x of
        Import y -> partitionTypes (y : is, as, ss) xr
        Assignment y -> partitionTypes (is, y : as, ss) xr
        Section y -> partitionTypes (is, as, y : ss) xr

-----------------------------------------------------------------------------

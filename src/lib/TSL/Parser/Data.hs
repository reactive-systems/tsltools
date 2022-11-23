-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.Parser.Data
-- Maintainer  :  Felix Klein
--
-- Common data used by the parser module.
module TSL.Parser.Data
  ( Specification (..),
    globalDef,
  )
where

-----------------------------------------------------------------------------

import TSL.Binding (Binding)
import TSL.Expression (Expr, ExprPos)
import TSL.Types (SectionType)
import Text.Parsec (alphaNum, char, letter, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (GenLanguageDef (..), LanguageDef)

-----------------------------------------------------------------------------

-- | The @Specification@ record contains all the data of a
-- specification that is extracted by the parsing process.
data Specification = Specification
  { -- | list of imports
    imports :: [(FilePath, String, ExprPos, ExprPos)],
    -- | The list of bindings of an identifier to any other
    -- expression.
    definitions :: [Binding String],
    -- | The list of sections elements, each containing the list
    -- of expressions for that respective section.
    sections :: [(SectionType, Expr String)]
  }

-----------------------------------------------------------------------------

-- | The language definition which is shared among all parsers.
globalDef ::
  LanguageDef a
globalDef =
  emptyDef
    { identStart = letter <|> char '_' <|> char '@',
      identLetter =
        alphaNum
          <|> char '_'
          <|> char '@'
          <|> char '\''
          <|> char '.',
      commentLine = "//",
      commentStart = "/*",
      commentEnd = "*/",
      nestedComments = True,
      caseSensitive = True
    }

-----------------------------------------------------------------------------

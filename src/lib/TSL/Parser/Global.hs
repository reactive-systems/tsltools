-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.Parser.Global
-- Maintainer  :  Felix Klein
--
-- Parsers for global definitions and formula sections.
module TSL.Parser.Global
  ( GlobalElement (..),
    assignmentParser,
    sectionParser,
    elementsParser,
  )
where

-----------------------------------------------------------------------------

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import TSL.Binding (Binding (..), BoundExpr (..))
import TSL.Expression (Expr (..), ExprPos (..))
import TSL.Parser.Data (globalDef)
import TSL.Parser.Expression (exprParser)
import TSL.Parser.Utils (identifier, positionParser)
import TSL.Types (SectionType (..))
import Text.Parsec
  ( ParsecT,
    alphaNum,
    char,
    lookAhead,
    many,
    many1,
    oneOf,
    sepBy,
    space,
    upper,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( GenLanguageDef (..),
    GenTokenParser,
    braces,
    makeTokenParser,
    natural,
    reserved,
    reservedNames,
    reservedOp,
    stringLiteral,
    whiteSpace,
  )

-----------------------------------------------------------------------------

data GlobalElement
  = Import (FilePath, String, ExprPos, ExprPos)
  | Assignment (Binding String)
  | Section (SectionType, [Expr String])

-----------------------------------------------------------------------------

elementsParser ::
  Parser GlobalElement
elementsParser =
  (~~) >> (importParser <|> sectionParser <|> assignmentParser)

-----------------------------------------------------------------------------

importParser ::
  Parser GlobalElement
importParser = do
  keyword "import"
  (file, p1) <- filepath
  keyword "as"
  (name, p2) <- modulename
  return $ Import (file, name, p1, p2)

-----------------------------------------------------------------------------

-- | Parses a list of formulas within a section labeled by 'name'.
sectionParser ::
  Parser GlobalElement
sectionParser = do
  t <- sectionTypeParser
  xs <- br $ sepBy (nonEmptyParser exprParser) $ rOp ";"
  return $ Section (t, catMaybes xs)
  where
    nonEmptyParser p =
      fmap return p <|> return Nothing

-----------------------------------------------------------------------------

sectionTypeParser ::
  Parser SectionType
sectionTypeParser =
  ( keyword "initially"
      >> ( (keyword "assume" >> return InitiallyAssume)
             <|> (keyword "guarantee" >> return InitiallyGuarantee)
         )
  )
    <|> ( keyword "always"
            >> ( (keyword "assume" >> afterParser AlwaysAssume)
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
assignmentParser ::
  Parser GlobalElement
assignmentParser = do
  (x, pos) <- identifier (~~)
  argumentsParser x pos
    <|> reminderParser x [] pos
  where
    argumentsParser x pos = do
      args <- many1 (identifier (~~))
      (~~)
      reminderParser x args $
        ExprPos (srcBegin pos) (srcEnd $ snd $ last args) Nothing

    reminderParser x args pos = do
      rOp "="
      es <- many1 exprParser
      rOp ";"
      return $
        Assignment
          Binding
            { bIdent = x,
              bArgs = args,
              bPos = pos,
              bVal = GuardedBinding es
            }

-----------------------------------------------------------------------------

tokenparser ::
  GenTokenParser String p Identity
tokenparser =
  makeTokenParser
    globalDef
      { opStart = oneOf "=;:",
        opLetter = oneOf "=;:",
        reservedOpNames = ["=", ";", ":"],
        reservedNames =
          [ "initially",
            "always",
            "assume",
            "guarantee",
            "after",
            "import",
            "as"
          ]
      }

-----------------------------------------------------------------------------

br ::
  Parser a -> Parser a
br = braces tokenparser

-----------------------------------------------------------------------------

rOp ::
  String -> Parser ()
rOp = reservedOp tokenparser

-----------------------------------------------------------------------------

(~~) ::
  Parser ()
(~~) = whiteSpace tokenparser

-----------------------------------------------------------------------------

keyword ::
  String -> Parser ()
keyword = void . reserved tokenparser

-----------------------------------------------------------------------------

filepath ::
  ParsecT String () Identity (String, ExprPos)
filepath =
  positionParser (~~) $ stringLiteral tokenparser

-----------------------------------------------------------------------------

modulename ::
  ParsecT String () Identity (String, ExprPos)
modulename =
  positionParser (~~) $ do
    f <- upper
    xr <- many (alphaNum <|> char '_')
    void $ lookAhead space
    return $ f : xr

-----------------------------------------------------------------------------

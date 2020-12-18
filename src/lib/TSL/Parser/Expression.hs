-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Parser.Expression
-- Maintainer  :  Felix Klein
--
-- Expression Parser.
--
-----------------------------------------------------------------------------

module TSL.Parser.Expression
  ( exprParser
  ) where

-----------------------------------------------------------------------------

import TSL.Expression
  ( Expr(..)
  , Expr'(..)
  , SrcPos(..)
  , ExprPos(..)
  )

import TSL.Parser.Data
  ( globalDef
  )

import TSL.Parser.Utils
  ( ch
  , getPos
  , identifier
  , positionParser
  )

import Control.Monad
  ( void
  )

import Text.Parsec
  ( (<|>)
  , try
  , oneOf
  , many
  , many1
  , digit
  , lookAhead
  , notFollowedBy
  , alphaNum
  , string
  )

import Text.Parsec.Expr
  ( Assoc(..)
  , Operator(..)
  , buildExpressionParser
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec.Token
  ( GenLanguageDef(..)
  , commaSep
  , reservedNames
  , whiteSpace
  , makeTokenParser
  , reserved
  , reservedOp
  )

-----------------------------------------------------------------------------

-- | Parses an expression.

exprParser
  :: Parser (Expr String)

exprParser = (~~) >> buildExpressionParser table term
  where
    table =
      [ [ Infix  applyFn                         AssocLeft
        ]

      , [ Prefix $ unaryOperators numUnary
        ]

      , [ Infix  (binOp "*"        NumMul)       AssocLeft
        , Infix  (binOp "MUL"      NumMul)       AssocLeft
        ]
      , [ Infix  (binOp "/"        NumDiv)       AssocRight
        , Infix  (binOp "DIV"      NumDiv)       AssocRight
        , Infix  (binOp "%"        NumMod)       AssocRight
        , Infix  (binOp "MOD"      NumMod)       AssocRight
        ]
      , [ Infix  (binOp "+"        NumPlus)      AssocLeft
        , Infix  (binOp "PLUS"     NumPlus)      AssocLeft
        , Infix  (binOp "-"        NumMinus)     AssocLeft
        , Infix  (binOp "MINUS"    NumMinus)     AssocLeft
        ]

      , [ Prefix $ unaryOperators setUnary
        ]

      , [ Infix  (binOp "(-)"      SetMinus)     AssocRight
        , Infix  (binOp "(\\)"     SetMinus)     AssocRight
        , Infix  (binOp "SETMINUS" SetMinus)     AssocRight
        ]
      , [ Infix  (binOp "(*)"      SetCap)       AssocLeft
        , Infix  (binOp "CAP"      SetCap)       AssocLeft
        ]
      , [ Infix  (binOp "(+)"      SetCup)       AssocLeft
        , Infix  (binOp "CUP"      SetCup)       AssocLeft
        ]
      , [ Infix  (binOp "=="       BlnEQ)        AssocLeft
        , Infix  (binOp "EQ"       BlnEQ)        AssocLeft
        , Infix  (binOp "/="       BlnNEQ)       AssocLeft
        , Infix  (binOp "!="       BlnNEQ)       AssocLeft
        , Infix  (binOp "NEQ"      BlnNEQ)       AssocLeft
        , Infix  (binOp ">"        BlnGE)        AssocLeft
        , Infix  (binOp "GE"       BlnGE)        AssocLeft
        , Infix  (binOp ">="       BlnGEQ)       AssocLeft
        , Infix  (binOp "GEQ"      BlnGEQ)       AssocLeft
        , Infix  (binOp "<"        BlnLE)        AssocLeft
        , Infix  (binOp "LE"       BlnLE)        AssocLeft
        , Infix  (binOp "<="       BlnLEQ)       AssocLeft
        , Infix  (binOp "LEQ"      BlnLEQ)       AssocLeft
        ]
      , [ Infix  (binOp "IN"       BlnElem)      AssocLeft
        , Infix  (binOp "ELEM"     BlnElem)      AssocLeft
        ]

      , [ Prefix $ unaryOperators tslUnary
        ]

      , [ Infix  (binOp "&&"       BlnAnd)       AssocLeft
        , Infix  (binOp "AND"      BlnAnd)       AssocLeft
        ]
      , [ Infix  (binOp "||"       BlnOr)        AssocLeft
        , Infix  (binOp "OR"       BlnOr)        AssocLeft
        ]
      , [ Infix  (binOp "->"       BlnImpl)      AssocRight
        , Infix  (binOp "IMPIES"   BlnImpl)      AssocRight
        , Infix  (binOp "<->"      BlnEquiv)     AssocRight
        , Infix  (binOp "EQUIV"    BlnEquiv)     AssocRight
        ]
      , [ Infix  (binOp "W"        TslWeak)      AssocRight
        , Infix  (binOp "A"        TslAsSoonAs)  AssocRight
        ]
      , [ Infix  (binOp "U"        TslUntil)     AssocRight
        ]
      , [ Infix  (binOp "R"        TslRelease)   AssocLeft
        ]
      , [ Infix  (binOp "S"        TslSince)     AssocLeft
        ]
      , [ Infix  (binOp "T"        TslTriggered) AssocLeft
        ]
      , [ Infix  (binOp "~"        Pattern)      AssocLeft
        ]
      , [ Infix  (binOp ":"        Colon)        AssocLeft
        ]
      ]

    tokenDef =
      globalDef
      { opStart = oneOf "!&|-<=/+*%(:~,.["
      , opLetter = oneOf "!&|<->=/\\[+*%():~,.]"
      , reservedOpNames =
          ["!","&&","||","->","<->","==","/=","<",">","<=",">=",
           "<-","&&[","||[","NOT","AND","OR","IMPLIES","EQUIV","EQ",
           "NEQ", "LE", "GE", "LEQ", "GEQ", "ELEM","AND[","OR[",
           "+","-","*","/","%","PLUS","MINUS","MUL","DIV","MOD",
           "SIZE","MIN","MAX","(-)","(\\)","(+)","(*)","SETMINUS",
           "CAP","CUP",":","~","W","A","U","R","X","Y","G","F","H",
           "O","S","T",",","X[","Y[","G[","F[","H[","O[","AND[",
           "OR[","SUM","PROD","IN"]
      , reservedNames =
          ["NOT","AND","OR","IMPLIES","EQUIV","true","false","F",
           "PLUS","MINUS","MUL","DIV","MOD","SIZE","MIN","MAX","_",
           "SETMINUS","CAP","CUP","otherwise","W","A","U","R","X",
           "G","SUM","PROD","IN","Y","H","O","S","T"]
      }

    tokenparser = makeTokenParser tokenDef

    term =
          parentheses
      <|> setExplicit
      <|> try (between' '|' '|' (fmap NumSSize exprParser))
      <|> keyword "otherwise" BaseOtherwise
      <|> keyword "false" BaseFalse
      <|> keyword "true" BaseTrue
      <|> keyword "_" BaseWild
      <|> constant
      <|> update
      <|> ident

    numUnary =
          unOp4 'S' 'I' 'Z' 'E' NumSSize
      <|> unOp3 'M' 'I' 'N' NumSMin
      <|> unOp3 'M' 'A' 'X' NumSMax
      <|> parOp "+" manyExprParser NumRPlus
      <|> parOp "SUM" manyExprParser NumRPlus
      <|> parOp "*" manyExprParser NumRMul
      <|> parOp "PROD" manyExprParser NumRMul

    setUnary =
          parOp "(+)" manyExprParser SetRCup
      <|> parOp "CUP" manyExprParser SetRCap
      <|> parOp "(-)" manyExprParser SetRCup
      <|> parOp "CAP" manyExprParser SetRCap

    tslUnary =
          unOp' '!' BlnNot
      <|> unOp3 'N' 'O' 'T' BlnNot
      <|> unOp1 'X' TslNext
      <|> unOp1 'Y' TslPrevious
      <|> unOp1 'G' TslGlobally
      <|> unOp1 'F' TslFinally
      <|> unOp1 'H' TslHistorically
      <|> unOp1 'O' TslOnce
      <|> parOp "X" exprParser TslRNext
      <|> parOp "Y" exprParser TslRPrevious
      <|> parOp "G" exprParser TslRGlobally
      <|> parOp "F" exprParser TslRFinally
      <|> parOp "H" exprParser TslRHistorically
      <|> parOp "O" exprParser TslROnce
      <|> parOp "&&" manyExprParser BlnRAnd
      <|> parOp "AND" manyExprParser BlnRAnd
      <|> parOp "FORALL" manyExprParser BlnRAnd
      <|> parOp "||" manyExprParser BlnROr
      <|> parOp "OR" manyExprParser BlnROr
      <|> parOp "EXISTS" manyExprParser BlnROr

    parentheses = do
      notFollowedBy $ ch '(' >> oneOf "+-*/"
      between' '(' ')' $ fmap expr exprParser

    keyword x c = do
      s <- getPos
      void $ reserved tokenparser x
      return
        Expr
          { expr = c
          , exprId = -1
          , srcPos =
              ExprPos
                { srcBegin = s
                , srcEnd = s
                    { srcColumn = srcColumn s + length x
                    }
                , srcPath = Nothing
                }
          }

    setExplicit = do
      s <- getPos; ch '{'; (~~)
      emptySet s <|> nonEmptySet s

    emptySet s = do
      e <- closeSet
      return
        Expr
          { expr = SetExplicit []
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    nonEmptySet s = do
      x <- exprParser
      singeltonSet s x <|> nonSingeltonSet s x

    singeltonSet s x = do
      e <- closeSet
      return
        Expr
          { expr = SetExplicit [x]
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    nonSingeltonSet s x = do
      ch ','; (~~)
      y <- exprParser
      twoElmSet s x y <|> rangeSet s x y <|> manyElmSet s x y

    twoElmSet s x y = do
      e <- closeSet
      return
        Expr
          { expr = SetExplicit [x,y]
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    rangeSet s x y = do
      ch '.'; ch '.'; (~~)
      z <- exprParser
      e <- closeSet
      return
        Expr
          { expr = SetRange x y z
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    manyElmSet s x y = do
      ch ','; (~~)
      xs <- manyExprParser
      e <- closeSet
      return
        Expr
          { expr = SetExplicit (x:y:xs)
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    closeSet = do { ch '}'; e <- getPos; (~~); return e }

    binOp x c = do
      reservedOp tokenparser x
      return $ \a b ->
        Expr
          { expr = c a b
          , exprId = -1
          , srcPos =
              ExprPos
                { srcBegin = srcBegin $ srcPos a
                , srcEnd = srcEnd $ srcPos b
                , srcPath = Nothing
                }
          }

    unaryOperators p = do
      (x:xr) <- many1 $ unaryOperator p
      return $ conUnOp x xr

    unaryOperator p = do
      s <- getPos
      c <- p
      return (s,c)

    conUnOp (s,c) xs = case xs of
      []     -> \e ->
        Expr
          { expr = c e
          , exprId = -1
          , srcPos =
              ExprPos
                { srcBegin = s
                , srcEnd = srcEnd $ srcPos e
                , srcPath = Nothing
                }
          }
      (x:xr) -> \e ->
        Expr
          { expr = c $ conUnOp x xr e
          , exprId = -1
          , srcPos =
              ExprPos
                { srcBegin = s
                , srcEnd = srcEnd $ srcPos e
                , srcPath = Nothing
                }
          }

    unOp4 c1 c2 c3 c4 c = try $ do
      ch4 c1 c2 c3 c4
      lookahead
      return c

    unOp' x c = do
      ch x
      (~~)
      return c

    unOp1 x c = try $ do
      ch x
      lookahead
      return c

    unOp3 c1 c2 c3 c = try $ do
      ch2 c1 c2
      ch c3
      lookahead
      return c

    parOp x p c = do
      reservedOp tokenparser (x ++ "[")
      e <- p; ch ']'; (~~)
      return (c e)

    between' c1 c2 p = do
      s <- getPos; ch c1; (~~); x <- p
      ch c2; e <- getPos; (~~)
      return
        Expr
          { expr = x
          , exprId = -1
          , srcPos = ExprPos s e Nothing
          }

    constant = do
      (x,pos) <- positionParser (~~) $ many1 digit
      return
        Expr
          { expr = BaseCon $ read x
          , exprId = -1
          , srcPos = pos
          }

    update =
      between' '[' ']' update'

    update' = do
      (i,_) <- identifier (~~)
      reservedOp tokenparser "<-"
      e <- exprParser
      return (BaseUpd e i)

    ident = do
      (i,pos) <- do
        a <- getPos
        x <- identStart tokenDef
        xr <- many $ identLetter tokenDef
        b <- getPos
        return (x:xr, ExprPos a b Nothing)

      constantFunctionParser pos i <|> ident' pos i

    ident' p i =
      (~~) >> return Expr
        { expr = BaseId i
        , exprId = -1
        , srcPos = p
        }

    constantFunctionParser (ExprPos s (SrcPos l c) _) i =
      try $ ch '(' >> ch ')' >> (~~) >> return Expr
        { expr = BaseConFn i
        , exprId = -1
        , srcPos = ExprPos s (SrcPos l $ c + 2) Nothing
        }

    applyFn = do
      notFollowedBy
        (reservedOpLiteral >> notFollowedBy alphaNum)
      return $ \a b ->
        Expr
          { expr = BaseFn a b
          , exprId = -1
          , srcPos =
              ExprPos
                { srcBegin = srcBegin $ srcPos a
                , srcEnd = srcEnd $ srcPos b
                , srcPath = Nothing
                }
          }

    reservedOpLiteral =
          string "W"
      <|> string "A"
      <|> string "U"
      <|> string "R"
      <|> string "T"
      <|> string "S"
      <|> string "OR"
      <|> string "AND"
      <|> string "IMPLIES"
      <|> string "EQUIV"
      <|> string "IN"
      <|> string "ELEM"
      <|> string "LEQ"
      <|> string "LE"
      <|> string "GEQ"
      <|> string "GE"
      <|> string "NEQ"
      <|> string "EQ"
      <|> string "CUP"
      <|> string "CAP"
      <|> string "SETMINUS"
      <|> string "MUL"
      <|> string "DIV"
      <|> string "MOD"
      <|> string "PLUS"
      <|> string "MINUS"

    manyExprParser = commaSep tokenparser exprParser

    (~~) = whiteSpace tokenparser

    lookahead = do
      lookAhead (ch ' ' <|> ch '(' <|> ch '\t' <|> ch '\n')
      (~~)

    ch2 c1 c2 = do { ch c1; ch c2 }
    ch4 c1 c2 c3 c4 = do { ch2 c1 c2; ch2 c3 c4 }

-----------------------------------------------------------------------------

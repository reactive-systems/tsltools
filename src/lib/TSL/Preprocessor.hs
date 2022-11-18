{-# LANGUAGE LambdaCase       #-}

-----------------------------------------------------------------------------
-- Imports

import Data.Functor.Identity (Identity)

import Control.Monad (liftM)

import Numeric (showFFloat)

import System.Directory (listDirectory)

import Text.Parsec.String (Parser)

import Text.Parsec (alphaNum, char, letter, (<|>))

import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)

import Text.Parsec.Language (emptyDef)

import Text.Parsec
  ( choice
  , digit
  , lookAhead
  , many
  , many1
  , sepBy
  , sepBy1
  , endBy
  , notFollowedBy
  , oneOf
  , try
  , unexpected
  , spaces
  , (<?>)
  , (<|>)
  )

import qualified Text.Parsec as Parsec

import qualified Text.Parsec.Token as Token

import System.Exit(die)

import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- Utility Functions

surround :: a -> a -> [a] -> [a]
surround left right inside = left:inside ++ [right]

parenthize :: String -> String
parenthize = surround '(' ')'

bracketify :: String -> String
bracketify = surround '[' ']'

-------------------------------------------------------------------------------

newtype Specification = Specification [Section]
  deriving (Show, Eq)

data Section = 
  Section (Maybe TemporalWrapper) SectionType [Expr]
  deriving (Show, Eq)

data TemporalWrapper = Initially | Always
  deriving (Show, Eq)

data SectionType = Assume | Guarantee
  deriving (Show, Eq)

data Expr =
    PredicateExpr Predicate
  | Update Signal Signal
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  deriving (Show, Eq)

data Predicate =
    BinaryPredicate BinaryComparator Signal Signal
  | UninterpretedPredicate String [Signal]
  deriving (Show)

data UnaryOp = Not | Next | Globally | Eventually
  deriving (Show, Eq)

data BinaryOp
  = And
  | Or 
  | Implies
  | Iff
  | Until
  | WeakUntil
  | Release
  deriving (Show, Eq)

data Signal
  = TSLInt Integer
  | TSLReal Double
  | Symbol String
  | BinaryFunction BinaryFunction Signal Signal
  | UninterpretedFunction String [Signal]
  deriving (Show)

data BinaryComparator
  = Eq
  | Lt
  | Gt
  | Lte
  | Gte
  deriving (Show, Eq)

data BinaryFunction
  = Add
  | Sub
  | Mult
  | Div
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Fmt instances

class Show a => Fmt a where
  fmt :: a -> String

instance (Fmt a) => Fmt [a] where
  fmt = concat . (map fmt)

instance Fmt Char where
  fmt c = [c]

instance Fmt Specification where
  fmt (Specification sections) = unlines $ map fmt sections

instance Fmt Section where
  fmt (Section temporalWrapper sectionType expressions) =
    unlines [header ++ " {", unlines (map fmtExpr expressions), "}"]
      where header   = (++ fmt sectionType) (case temporalWrapper of
                                              Nothing -> ""
                                              Just tw -> fmt tw ++ " ")
            fmtExpr = (++";") . fmt

instance Fmt TemporalWrapper where
  fmt = \case
    Initially -> "initially"
    Always    -> "always"

instance Fmt SectionType where
  fmt = \case
    Assume    -> "assume"
    Guarantee -> "guarantee"

instance Fmt Expr where
  fmt = \case
    PredicateExpr p -> fmt p
    Update dst src          -> bracketify $ unwords [fmt dst, "<-", fmt src]
    Unary op expr           -> unwords [fmt op, fmt expr]
    Binary op ex1 ex2       -> unwords [fmt ex1, fmt op, fmt ex2]

instance Fmt Predicate where
  fmt = \case
    BinaryPredicate comp lhs rhs  -> parenthize $ unwords [fmt comp, fmt lhs, fmt rhs]
    UninterpretedPredicate p args -> parenthize $ unwords $ p:(map fmt args)

instance Fmt UnaryOp where
  fmt = \case
    Not        -> "!"
    Next       -> "X"
    Globally   -> "G"
    Eventually -> "F"

instance Fmt BinaryOp where
  fmt = \case
    And       -> "&&"
    Or        -> "||"
    Implies   -> "->"
    Iff       -> "<->"
    Until     -> "U"
    WeakUntil -> "W"
    Release   -> "R"

instance Fmt Signal where
  fmt = \case
    TSLInt  s                      -> "int"  ++ show s ++ "()"
    TSLReal s                      -> "real" ++ (showFFloat Nothing s "") ++ "()"
    Symbol  s                      -> s
    BinaryFunction f lhs rhs       -> parenthize $ unwords [fmt f, fmt lhs, fmt rhs]
    UninterpretedFunction f args   -> parenthize $ unwords $ f:(map fmt args)

instance Fmt BinaryFunction where
  fmt = \case
    Add  -> "add"
    Sub  -> "sub"
    Mult -> "mult"
    Div  -> "div"

instance Fmt BinaryComparator where
  fmt = \case
    Eq   -> "eq"
    Lt   -> "lt"
    Gt   -> "gt"
    Lte  -> "lte"
    Gte  -> "gte"
---------------------------------------------------------------------------
-- Eq instances

instance Eq Predicate where
  (==) = \case
    (BinaryPredicate comp lhs rhs) -> \case
      (BinaryPredicate comp' lhs' rhs')       ->
        comp == comp' && lhs == lhs' && rhs == rhs'
      (UninterpretedPredicate p [lhs', rhs']) ->
        (fmt comp) == p && lhs == lhs' && rhs == rhs'
      _                                       -> False

    u@(UninterpretedPredicate p1 args1) -> \case
      (UninterpretedPredicate p2 args2) -> p1 == p2 && args1 == args2
      binary                            -> binary == u

instance Eq Signal where
  int@(TSLInt     _) == other = fmt int    == fmt other
  real@(TSLReal   _) == other = fmt real   == fmt other

  symbol@(Symbol s) == (UninterpretedFunction f []) = s == f
  symbol@(Symbol _) == other = fmt symbol == fmt other

  (BinaryFunction f lhs rhs) == (BinaryFunction g lhs' rhs') =
   f == g && lhs == lhs' && rhs == rhs'
  (BinaryFunction f lhs rhs) == (UninterpretedFunction g [lhs', rhs']) = 
   fmt f == g && lhs == lhs' && rhs == rhs'
  (BinaryFunction f lhs rhs) == _ = False

  (UninterpretedFunction f args) == (UninterpretedFunction g args') =
    f == g && args == args'
  uninterpreted@(UninterpretedFunction _ _) == other = other == uninterpreted
---------------------------------------------------------------------------
-- Lexer

binOpNames :: [String]
binOpNames = ["=" ,"<" ,">" ,"<=" ,">=" ,"+" ,"-" ,"*" ,"/"]

sectionNames :: [String]
sectionNames = ["initially", "always", "assume", "guarantee"]

temporalOpNames :: [String]
temporalOpNames = ["R","U","W","X","F"]

tslDef :: Token.LanguageDef a
tslDef =
  emptyDef
  { Token.identStart      = char '_' <|> letter 
  , Token.identLetter     = alphaNum <|> oneOf "._"
  , Token.commentLine     = "//"
  , Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.nestedComments  = True
  , Token.caseSensitive   = True
  , Token.opStart         = oneOf "!&|=/+*[-<"
  , Token.opLetter        = oneOf "!&|=/+*[]<->"
  , Token.reservedNames   = sectionNames ++ temporalOpNames
  , Token.reservedOpNames = binOpNames
  }

lexer :: Token.GenTokenParser String p Identity
lexer = Token.makeTokenParser tslDef

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: (Show a, Fmt a) => Parser a -> Parser a
parens = (debugger "PARENS") . Token.parens lexer

braces :: (Show a, Fmt a) => Parser a -> Parser a
braces = (debugger "BRACES") . Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

semicolon :: Parser ()
semicolon = Token.semi lexer >> return ()

exprOperators :: [[Operator String () Identity Expr]]
exprOperators =
  [
   [ Prefix  (reservedOp "!"  >> return (Unary Not        )) 
   , Prefix  (reserved   "X"  >> return (Unary Next       ))
   , Prefix  (reserved   "G"  >> return (Unary Globally   ))
   , Prefix  (reserved   "F"  >> return (Unary Eventually )) 
   ]
  ,[ Infix  (reservedOp "&&"  >> return (Binary And       )) AssocLeft
   , Infix  (reservedOp "||"  >> return (Binary Or        )) AssocLeft
   , Infix  (reservedOp "->"  >> return (Binary Implies   )) AssocLeft
   , Infix  (reservedOp "<->" >> return (Binary Iff       )) AssocLeft
   , Infix  (reserved   "U"   >> return (Binary Until     )) AssocLeft
   , Infix  (reserved   "W"   >> return (Binary WeakUntil )) AssocLeft
   , Infix  (reserved   "R"   >> return (Binary Release   )) AssocLeft
   ]
  ]

binaryFunctions :: [[Operator String () Identity Signal]]
binaryFunctions =
  [
   [ Infix  (reservedOp "+"  >> return (BinaryFunction Add )) AssocLeft
   , Infix  (reservedOp "-"  >> return (BinaryFunction Sub )) AssocLeft
   , Infix  (reservedOp "*"  >> return (BinaryFunction Mult)) AssocLeft
   , Infix  (reservedOp "/"  >> return (BinaryFunction Div )) AssocLeft
   ]
  ]

---------------------------------------------------------------------------
-- Parser

data DebugMode = 
    Silent
  | Quiet
  | Normal
  | Verbose

debugger :: (Show a, Fmt a) => String -> Parser a -> Parser a
debugger description parser = case debugMode of
  Silent -> parser
  Quiet -> do
    val <- parser
    trace (msg val) $ return val
  Normal -> do
   val <- parser 
   Parsec.parserTrace $ msg val
   return val
  Verbose -> do
   Parsec.parserTrace $ "\n>>> BEFORE " ++ description ++ ", Remaining"
   val <- parser
   Parsec.parserTrace $ msg val
   Parsec.parserTrace $ "\n<<< AFTER " ++ description ++ ", Remaining"
   return val
  where msg value = "\n{{" ++ description ++ "}}\n" ++
                    "\n  Format:\n\t" ++ fmt value ++
                    "\n  Show:\n\t" ++ show value ++
                    "\n  Remaining"

specParser :: Parser Specification
specParser = whiteSpace >> (liftM Specification $ sectionParser `sepBy` spaces)

sectionParser :: Parser Section
sectionParser = do
  temporalWrapper <- temporalParser
  sectionType     <- sectionTypeParser
  exprs           <- braces $ (debugger "EXPRS IN SECTION") $ exprParser `endBy` semicolon
  return $ Section temporalWrapper sectionType exprs
  where
    temporalParser :: Parser (Maybe TemporalWrapper)
    temporalParser =  (reserved "initially" >> return (Just Initially))
                  <|> (reserved "always"    >> return (Just Always))
                  <|> (return Nothing)

    sectionTypeParser :: Parser SectionType
    sectionTypeParser =  (reserved "guarantee" >> return Guarantee)
                     <|> (reserved "assume"    >> return Assume)

exprParser :: Parser Expr
exprParser = (debugger "EXPR") $ buildExpressionParser exprOperators exprTerm

exprPrefixParser :: Parser Expr
exprPrefixParser = prefixParser <*> exprParser
  where prefixParser :: Parser (Expr -> Expr)
        prefixParser =  (reservedOp "!"  >> return (Unary Not))
                    <|> (reserved   "X"  >> return (Unary Next))
                    <|> (reserved   "G"  >> return (Unary Globally))
                    <|> (reserved   "F"  >> return (Unary Eventually))

exprTerm :: Parser Expr
exprTerm = (debugger "EXPR TERM") $ updateTerm
           <|> try (liftM PredicateExpr predicateParser)
           <|> parens exprParser
           <|> exprPrefixParser

predicateParser :: Parser Predicate
predicateParser = (debugger "PREDICATE") $
                  (try interpreted)
                <|> uninterpreted
  where
    uninterpreted :: Parser Predicate
    uninterpreted = liftM (uncurry UninterpretedPredicate) functionLiteralParser

    interpreted :: Parser Predicate
    interpreted   = do
      lhs        <- (debugger "PRED_LHS") signalParser
      comparator <- (debugger "COMPARATOR") comparatorParser
      rhs        <- (debugger "PRED_RHS") signalParser
      return $ BinaryPredicate comparator lhs rhs

comparatorParser :: Parser BinaryComparator
comparatorParser =  (reservedOp "="  >> return Eq )
                <|> (reservedOp "<=" >> return Lte)
                <|> (reservedOp ">=" >> return Gte)
                <|> (reservedOp "<"  >> return Lt )
                <|> (reservedOp ">"  >> return Gt )

updateTerm :: Parser Expr
updateTerm = (debugger "UPDATE") $ brackets $ do
  dst <- identifier
  _   <- reservedOp "<-"
  src <- signalParser
  return $ Update (Symbol dst) src

signalParser :: Parser Signal
signalParser = (debugger "SIGNAL") $ buildExpressionParser binaryFunctions signalTerm

signalTerm :: Parser Signal
signalTerm = (debugger "SIGNAL TERM") $ parens signalParser
           <|> try (liftM TSLReal float)
           <|> liftM TSLInt integer
           <|> try constantParser
           <|> try (liftM (uncurry UninterpretedFunction) functionLiteralParser)
           <|> liftM Symbol identifier

constantParser :: Parser Signal
constantParser = do
  symbol  <- identifier
  nullary <- Parsec.string "()" 
  _       <- spaces
  return $ Symbol $ symbol ++ nullary

functionLiteralParser :: Parser (String, [Signal])
functionLiteralParser = do
  function <- (debugger "FUNCTION") identifier
  args     <- (debugger "ARGS") $ many argParser
  return (function, args)
  where argParser =  try constantParser
                 <|> liftM Symbol (try identifier)
                 <|> try signalParser

preprocess :: String -> Either Parsec.ParseError Specification
preprocess input = Parsec.parse (specParser <* Parsec.eof) errMsg input
  where errMsg = "\n\nParser Failed! Input was:\n\n" ++
                   (unlines (map ('\t':) (lines input)))
                   ++ "\n\n"

---------------------------------------------------------------------------

parseA :: (Fmt a) => Parser a -> String -> IO ()
parseA parser input =
  case parsed of
    Left err  -> die $ "FAIL: " ++ show err ++ "\n---\n"
    Right res -> case debugMode of
      Silent -> putStr "SUCCESS!!"
      _      -> putStrLn $ "SUCCESS!" ++
        "\nBEFORE:\n" ++ input ++
        "\nAFTER :\n" ++ fmt res  ++ "\n---\n" ++ show res ++ "\n---\n"
  where parsed = Parsec.parse (parser <* Parsec.eof) errMsg input
        errMsg = "\n\nParser Failed! Input was:\n\n" ++
                   (unlines (map ('\t':) (lines input)))
                   ++ "\n\n"

runTest :: FilePath -> IO ()
runTest dir = do
  inputTSL    <- readFile inputPath
  expectedTSL <- readFile expectedPath
  case preprocess inputTSL of
    Left err     -> die $ "ACTUAL FAIL: " ++ show err ++ "\n---\n"
    Right actual -> case preprocess expectedTSL of
      Left err       -> die $ "EXPECTED FAIL: " ++ show err ++ "\n---\n"
      Right expected -> if actual == expected
        then putStrLn $ success inputTSL actual
        else die $ "FAIL!!\n" ++ (showParsed "Actual:" actual) ++ (showParsed "Expected:" expected)
  where inputPath            = dir ++ "/" ++ "input.tsl"
        expectedPath         = dir ++ "/" ++ "expected.tsl"
        starter              = "\n>>>\n"
        ender                = "\n<<<\n"
        showParsed desc val  = unlines [starter, desc, fmt val, show val, ender]
        success input actual = (++ "\n\n") $ "Success!" ++ case debugMode of
          Silent  -> ""
          Quiet   -> "\n" ++ fmt actual
          Normal  -> "\nBefore:\n" ++ input ++
                     "\nAfter:\n" ++ fmt actual
          Verbose -> "\nBefore:\n" ++ input ++
                     "\nAfter:\n" ++ fmt actual ++
                     "\nDebug:\n" ++ show actual

unitTestDirs :: [FilePath]
unitTestDirs = map (((dir_base ++ "/") ++) . show) [0..6]
  where dir_base = "/Users/wonhyuk/tsltools/src/test/regression/Preprocess"

unitTests :: IO ()
unitTests = mapM_ runTest unitTestDirs

specDirs :: IO [FilePath]
specDirs = do
  fileNames <- listDirectory dir_base
  return $ map ((dir_base ++ "/") ++) $ filter (not . (flip elem) blackList) fileNames
  where dir_base  = "/Users/wonhyuk/tsltools/src/test/res/specs"
        blackList = [ "Caching.tsl"
                    , "NoteButton.tsl"
                    ]

specTests :: IO ()
specTests = do
  paths <- specDirs
  files <- mapM readFile paths
  mapM_ (parseA specParser) files

ex = "assume { const1() = const2(); const2() = const1(); }"

doEx = False

main :: IO ()
main = if doEx
          then parseA specParser ex
          else unitTests >> specTests

debugMode :: DebugMode
debugMode = Silent

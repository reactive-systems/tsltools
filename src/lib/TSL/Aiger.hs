-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Aiger
-- Maintainer  :  Felix Klein
--
-- Aiger Circuit Representation.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module TSL.Aiger
  ( Circuit(..)
  , Invertible(..)
  , Input(..)
  , Output(..)
  , Gate(..)
  , Latch(..)
  , Wire(..)
  , parseAag
  ) where

-----------------------------------------------------------------------------

import Data.Array
  ( array
  , (!)
  )

import Data.Array.ST
  ( newListArray
  , writeArray
  , runSTArray
  )

import TSL.Error
  ( Error
  , parseError
  , errFormat
  )

import Control.Monad
  ( unless
  , void
  )

import Control.Arrow
  ( (>>>)
  , second
  )

import Data.Graph
  ( topSort
  , buildG
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec
  ( manyTill
  , many
  , many1
  , count
  , lookAhead
  , try
  , string
  , newline
  , anyChar
  , char
  , digit
  , oneOf
  , parse
  , eof
  )

-----------------------------------------------------------------------------

-- | Data type to distinguish between a positive or a negative variant
-- of the enclosed type.

data Invertible a =
    Positive a
  | Negative a
  deriving (Eq, Ord)

-----------------------------------------------------------------------------

instance Show a => Show (Invertible a) where
  show = \case
    Positive x -> show x ++ "+"
    Negative x -> show x ++ "-"

-----------------------------------------------------------------------------

instance Functor Invertible where
  fmap f = \case
    Positive x -> Positive $ f x
    Negative x -> Negative $ f x

-----------------------------------------------------------------------------

newtype Input =
  Input
    { input :: Int
    }
  deriving (Ord, Eq)

instance Show Input where
  show = show . input

-----------------------------------------------------------------------------

newtype Output =
  Output
    { output :: Int
    }
  deriving (Ord, Eq)

instance Show Output where
  show = show . output

-----------------------------------------------------------------------------

newtype Wire =
  Wire
    { wire :: Int
    }
  deriving (Ord, Eq)

instance Show Wire where
  show = show . wire

-----------------------------------------------------------------------------

newtype Latch =
  Latch
    { latch :: Int
    }
  deriving (Ord, Eq)

instance Show Latch where
  show = show . latch

-----------------------------------------------------------------------------

newtype Gate =
  Gate
    { gate :: Int
    }
  deriving (Ord, Eq)

instance Show Gate where
  show = show . gate

-----------------------------------------------------------------------------

-- | The datatype represents a circut as a set of elements that are
-- connected by wires. Every element consists of a defined number of
-- inputs and outputs connection. The given mappings associate each
-- such connection with a unique wire. If the same wire is assigned to
-- connections of different components, then the two points are
-- connected. Additionally, every element is assigned a name for
-- better human readable identification.

data Circuit =
  Circuit
    { -- | number of inputs
      inputs :: [Input]

    , -- | number of outputs
      outputs :: [Output]

    , -- | number of latches
      latches :: [Latch]

    , -- | number of gates
      gates :: [Gate]

    , -- | number of wires
      wires :: [Wire]

    , -- | returns the wire (or its negation) that is connected to the
      -- given output
      outputWire :: Output -> Invertible Wire

    , -- | returns the wire that is connected to the given input
      inputWire :: Input -> Wire

    , -- | returns the wire (or its negation) that is connected to the
      -- input of the given latch
      latchInput :: Latch -> Invertible Wire

    , -- | returns the wire that is connected to the output of the
      -- given latch
      latchOutput :: Latch -> Wire

    , -- | returns the wire (or its negation) that is connected to the
      -- first input of the given gate
      gateInputA :: Gate -> Invertible Wire

    , -- | returns the wire (or its negation) that is connected to the
      -- second input of the given gate
      gateInputB :: Gate -> Invertible Wire

    , -- | returns the wire that is connected to the output of the
      -- given gate
      gateOutput :: Gate -> Wire

    , -- | returns the name assigned to the given input
      inputName :: Input -> String

    , -- | returns the name assigned to the given output
      outputName :: Output -> String

    , -- | returns the name assigned to the given latch
      latchName :: Latch -> String

    , -- | returns the order of the gates according to their
      -- dependencies
      gateDepOrder :: [Gate]
    }

-----------------------------------------------------------------------------

-- | Parses an AIG circuit in the AAG format. If the provided format
-- is incorrect, an error message is returned.

parseAag
  :: String -> Either Error Circuit

parseAag =
  parse aagParser "Aiger Circuit" >>> \case
    Left x                                      -> parseError x
    Right ((m,i,l,o,a),is,ls,os,as,ins,lns,ons) -> do
      check (is == [0,1..i - 1])
        $ "Missing inputs " ++ show is
      check (map fst os == [0,1..o - 1])
        $ "Missing Outputs " ++ show (map fst os)
      check (map fst ls == [0,1..l - 1])
        $ "Missing Latches " ++ show (map fst ls)
      check (map fst as == [0,1..a - 1])
        $ "Missing Gates " ++ show (map fst as)

      mapM_ (checkInvertible m) $ concat
        $ map snd os ++ map snd ls ++ map snd as

      let
        gOutput = Wire . (+1) . (+i) . (+l) . gate
        gInputA = aMap a gate (!! 0) as
        gInputB = aMap a gate (!! 1) as

      return
        Circuit
          { wires        = map Wire [0,1..m]
          , inputs       = map Input [0,1..i - 1]
          , latches      = map Latch [0,1..l - 1]
          , outputs      = map Output [0,1..o - 1]
          , gates        = map Gate [0,1..a - 1]
          , inputWire    = Wire . (+1) . input
          , latchOutput  = Wire . (+1) . (+i) . latch
          , gateOutput   = gOutput
          , outputWire   = aMap o output (!! 0) os
          , latchInput   = aMap l latch (!! 0) ls
          , gateInputA   = gInputA
          , gateInputB   = gInputB
          , inputName    = nMap i input ins
          , latchName    = nMap l latch lns
          , outputName   = nMap o output ons
          , gateDepOrder =
              map Gate $ topSort $ buildG (0, a - 1)
                [ (g', g)
                | g <- [0,1..a - 1]
                , g' <- [0,1..a - 1]
                , let w = gOutput (Gate g')
                , w == extract (gInputA (Gate g)) ||
                  w == extract (gInputB (Gate g))
                ]
          }

  where
    aMap n idx f xs =
      ((array (0,n - 1) $ map (second f) xs) !) . idx

    nMap n idx names =
      let
        aa = runSTArray $ do
          a <- newListArray (0,n - 1) (map show [0,1..n - 1])
          mapM_ (uncurry (writeArray a)) names
          return a
      in
        (aa !) . idx

    checkInvertible m x =
      check (wire (extract x) >= 0 && wire (extract x) <= m)
      $ "Wire index out of range: " ++ show (extract x)

    check b str =
      unless b $ perror str

    perror =
      errFormat . ("AAG Input Error - " ++)

    extract = \case
      Positive x -> x
      Negative x -> x

-----------------------------------------------------------------------------

aagParser
  :: Parser
      ( (Int, Int, Int, Int, Int)
      , [Int]
      , [(Int, [Invertible Wire])]
      , [(Int, [Invertible Wire])]
      , [(Int, [Invertible Wire])]
      , [(Int, String)]
      , [(Int, String)]
      , [(Int, String)]
      )

aagParser = do
  void $ string "aag"
  [m,i,l,o,a] <- count 5 (char ' ' >> numParser)
  void newline
  is <- count i inputParser
  ls <- count l $ latchParser i
  os' <- count o outputParser
  as <- count a $ gateParser i l
  inames <- many $ nameParser 'i'
  lnames <- many $ nameParser 'l'
  onames <- many $ nameParser 'o'
  void $ many commentLine

  let os = zip [0,1..o-1] os'

  return ((m,i,l,o,a),is,ls,os,as,inames,lnames,onames)

  where
    evenNumParser = do
      ds <- many (try (do {x <- digit; void $ lookAhead digit; return x }))
      l <- oneOf ['0','2','4','6','8']
      return (read (ds ++ [l]) :: Int)

    numParser = do
      ds <- many1 digit
      return (read ds :: Int)

    inputParser = do
      x <- evenNumParser
      void newline
      return ((x `div` 2) - 1)

    latchParser i = do
      (x,y) <- twoNumParser
      void newline
      return ((x `div` 2) - 1 - i, [pWire y])

    outputParser = do
      x <- numParser
      void newline
      return [pWire x]

    gateParser i l = do
      (x,y) <- twoNumParser
      void $ char ' '
      z <- numParser
      void newline
      return ((x `div` 2) - 1 - i - l, [pWire y, pWire z])

    twoNumParser = do
      x <- evenNumParser
      void $ char ' '
      y <- numParser
      return (x,y)

    nameParser c = do
      void $ char c
      i <- numParser
      void $ char ' '
      s <- manyTill anyChar newline
      return (i, s)

    commentLine = do
      void $ char 'c'
      manyTill anyChar eof

    pWire x
      | x == 0         = Negative $ Wire 0
      | x == 1         = Positive $ Wire 0
      | even x        = Positive $ Wire (x `div` 2)
      | otherwise     = Negative $ Wire (x `div` 2)

-----------------------------------------------------------------------------

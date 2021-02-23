----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Felix Klein
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module Test
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( Progress(..)
  , Result(..)
  , Test(..)
  , TestInstance(..)
  )

import TSL
  ( PredicateTerm
  , SignalTerm
  , decodeInputAP
  , decodeOutputAP
  , encodeInputAP
  , encodeOutputAP
  , fromString
  )


import qualified SplitTests (tests)

import qualified DependencyTests (tests)

import Test.QuickCheck
  ( Arbitrary
  , Result(..)
  , arbitrary
  , choose
  , listOf
  , quickCheckResult, Gen, oneof, vector
  )

import Data.Char (chr, ord)
import Data.List (intercalate)

-----------------------------------------------------------------------------

-- | String wrapper to create special arbitrary instance for identifiers.

newtype Identifier = Identifier { identifier :: String }
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------

-- | Arbitrary instance for identifiers.

instance Arbitrary Identifier where
  arbitrary = do
    -- TODO this is very hacky and should be changed
    x <- oneof [choose (10 :: Int, 35 :: Int), choose (37 :: Int, 40 :: Int), choose (44 :: Int, 49 :: Int), choose (51 :: Int, 52 :: Int), return (57 :: Int), return (61 :: Int)]
    xr <- listOf $ choose (0 :: Int, 65 :: Int)
    return $ Identifier $ map toC $ x : xr

    where
      toC c
        | c < 10          = chr (ord '0' + c)
        | c >= 10 && c < 36 = chr (ord 'a' + (c - 10))
        | c >= 36 && c < 62 = chr (ord 'A' + (c - 36))
        | c == 62          = '_'
        | c == 63          = '@'
        | c == 64          = '.'
        | otherwise       = '\''

-----------------------------------------------------------------------------

-- | 

updateGenerator
  :: Gen String
updateGenerator = do
  a <- arbitrary
  b <- arbitrary 
  return ("[ " ++ identifier a ++ " <- " ++ identifier b ++ " ]")

formulaGenerator
  :: Gen Formula
formulaGenerator = do
  f <- oneof  [updateGenerator] -- TODO add more kinds of grammar constructs
  return Formula { formula = f }

newtype Formula = Formula { formula :: String }
  deriving (Show, Ord, Eq)
  
instance Arbitrary Formula where
  arbitrary = formulaGenerator

specGenerator
  :: Gen Specification 
specGenerator = do
  i <- choose (1::Int, 10::Int)
  guarantees <- vector i
  return Specification { spec =
       "guarantee {\n"
    ++ intercalate ";\n" (map formula guarantees)
    ++ "\n}" 
    }

newtype Specification = Specification  { spec :: String }
  deriving (Ord, Eq)
  
instance Show Specification where
  show = spec
  
instance Arbitrary Specification  where
  arbitrary = specGenerator


propParse
  :: Specification -> Bool
propParse s = 
  case fromString (spec s) of
    Right _ -> True
    Left _ -> False

-----------------------------------------------------------------------------

-- | The encoding of TSL predicates as atomic input propositions and
-- the corresponing decoding are compatible with each other.

propReadInput
  :: PredicateTerm Identifier -> Bool

propReadInput p =
  case decodeInputAP $ encodeInputAP identifier p of
    Right x -> x == fmap identifier p
    Left _  -> False

-----------------------------------------------------------------------------

-- | The encoding or TSL updates as atomic output propositions and the
-- corresponing decoding are compatible with each other.

propReadOutput
  :: (Identifier, SignalTerm Identifier) -> Bool

propReadOutput (o,s) =
  case decodeOutputAP $ encodeOutputAP identifier o s of
    Right x -> x == (identifier o, fmap identifier s)
    Left _  -> False

-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return $
  [ test "QuickCheck: Read Input" qc01
  , test "QuickCheck: Read Output" qc02
  , test "QuickCheck: Parse Spec" qc03
  ]
  ++ SplitTests.tests
  ++ DependencyTests.tests

  where
    qc01 =
      quickCheckResult propReadInput >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x

    qc02 =
      quickCheckResult propReadOutput >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x

    qc03 =
      quickCheckResult propParse >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x


    test testname run =
      let
        t =
          TestInstance
            { run = run
            , name = testname
            , tags = []
            , options = []
            , setOption = \_ _ -> Right t
            }
      in
        Test t

-----------------------------------------------------------------------------

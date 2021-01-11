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
  , fromTSL
  , split
  , toTSL
  )

import SplitTests (splitTests)

import Test.QuickCheck
  ( Arbitrary
  , Result(..)
  , arbitrary
  , choose
  , listOf
  , quickCheckResult
  )

import Data.Char (isSpace)

import Data.Char (chr, ord)

-----------------------------------------------------------------------------

-- | String wrapper to create special arbitrary instance for identifiers.

newtype Identifier = Identifier { identifier :: String }
  deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------

-- | Arbitrary instance for identifiers.

instance Arbitrary Identifier where
  arbitrary = do
    x <- choose (10 :: Int, 61 :: Int)
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
  ] ++ map splitTest (zip [1 :: Int, 2..] splitTests)

  where
    qc01 =
      quickCheckResult propReadInput >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x

    qc02 =
      quickCheckResult propReadOutput >>= \case
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

    splitTest (i,(spec, splits)) =
      let
        x =
          TestInstance
            { run = do
                fromTSL Nothing spec >>= \case
                  Left _ -> do
                    putStrLn $ "Incorrect Specification:\n\n" ++ spec
                    return $ Finished $ Fail $
                      "Split test " ++ show i ++ " failed."
                  Right s -> case splitTest' (s, splits) of
                    Right () -> return $ Finished Pass
                    Left err -> do
                      putStrLn err
                      return $ Finished $ Fail $
                        "Split test " ++ show i ++ " failed."
            , name = "split" ++ show i
            , tags = []
            , options = []
            , setOption = \_ _ -> Right x
            }
      in
        Test x


    splitTest' (spec, splits) =
      let specs = split spec
      in if (length specs == length splits)
      then mapM_ check $ zip splits $ map toTSL specs
      else Left $ "Expected " ++ show (length splits) ++
                  " many splits, " ++ "but got " ++
                  show (length specs) ++ "."

    check (exp, res)
      | trim exp == trim res = return ()
      | otherwise =
          Left $ "Expected:\n\n" ++ exp ++ "\nbut Got:\n\n" ++ res ++ "\n"

    trim = f . f
    f = reverse . dropWhile isSpace

-----------------------------------------------------------------------------

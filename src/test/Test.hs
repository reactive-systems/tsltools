----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Felix Klein
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Test
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite

import TSL.Logic
  ( PredicateTerm
  , SignalTerm
  , encodeAPInput
  , encodeAPOutput
  , decodeAPInput
  , decodeAPOutput
  )

import TSL.Reader
  ( fromTSLtoTSLSpec
  )

import TSL.Splitter
  ( split
  )

import TSL.ToString
  ( tslSpecToString
  )

import Test.QuickCheck
  ( Result(..)
  , Arbitrary
  , quickCheckResult
  , arbitrary
  , choose
  , listOf
  )

import Data.Char
  ( isSpace
  )

import Data.Char
  ( chr
  , ord
  )

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
  case decodeAPInput $ encodeAPInput identifier p of
    Right x -> x == fmap identifier p
    Left _  -> False

-----------------------------------------------------------------------------

-- | The encoding or TSL updates as atomic output propositions and the
-- corresponing decoding are compatible with each other.

propReadOutput
  :: (Identifier, SignalTerm Identifier) -> Bool

propReadOutput (o,s) =
  case decodeAPOutput $ encodeAPOutput identifier o s of
    Right x -> x == (identifier o, fmap identifier s)
    Left _  -> False

-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return
  [ test "QuickCheck: Read Input" qc01
  , test "QuickCheck: Read Output" qc02
  , test "split01" split01
  , test "split02" split02
  , test "split03" split03
  , test "split04" split04
  , test "split05" split05
  , test "split06" split06
  , test "split07" split07
  , test "split08" split08
  , test "split09" split09
  , test "split10" split10
  ]

  where
    qc01 = do
      putStrLn ""
      quickCheckResult propReadInput >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x

    qc02 = do
      putStrLn ""
      quickCheckResult propReadOutput >>= \case
        Success{..} -> return $ Finished Pass
        x           -> return $ Finished $ Fail $ show x

    split01 = do
      putStrLn ""
      splitTest "test/test_01.tsl" ["test/test_01A.tsl", "test/test_01B.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split02 = do
      putStrLn ""
      splitTest "test/test_02.tsl" ["test/test_02A.tsl",
                "test/test_02B.tsl", "test/test_02C.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split03 = do
      putStrLn ""
      splitTest "test/test_03.tsl" ["test/test_03A.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split04 = do
      putStrLn ""
      splitTest "test/test_04.tsl" ["test/test_04A.tsl",
                "test/test_04B.tsl", "test/test_04C.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split05 = do
      putStrLn ""
      splitTest "test/test_05.tsl" ["test/test_05A.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split06 = do
      putStrLn ""
      splitTest "test/test_06.tsl" ["test/test_06A.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split07 = do
      putStrLn ""
      splitTest "test/test_07.tsl" ["test/test_07A.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split08 = do
      putStrLn ""
      splitTest "test/test_08.tsl" ["test/test_08A.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split09 = do
      putStrLn ""
      splitTest "test/test_09.tsl" ["test/test_09A.tsl", "test/test_09B.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

    split10 = do
      putStrLn ""
      splitTest "test/test_10.tsl" ["test/test_10A.tsl", "test/test_10B.tsl"] >>= \case
        (True, _)   -> return $ Finished Pass
        (False, x)  -> do
                    putStrLn x
                    return $ Finished $ Fail "split did not return the expected result"

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

splitTest
  :: FilePath -> [FilePath] -> IO (Bool, String)
splitTest specPath expPaths = do
  str <- readFile specPath
  case fromTSLtoTSLSpec str of
    Left _ -> do
      return (False, "incorrect specification")
    Right s  -> do
      let specs = split s
      let expLength = length expPaths
      case assertEqual expLength (length specs) of
        (True, _)   -> do
          expSpecs <- mapM readFile expPaths
          return $ foldr (\(e, a) -> \(b, s) -> if b then assertStringEqual e a else (b, s)) (True, "")
            $ zip expSpecs $ map tslSpecToString specs
        r           -> return r

assertEqual :: (Eq a, Show a) => a -> a -> (Bool, String)
assertEqual exp act = if exp == act then (True, "")
  else (False, "expected:\n" ++ show exp ++ "\nwas:\n" ++ show act)

assertStringEqual :: String -> String -> (Bool, String)
assertStringEqual exp act = if trim exp == trim act then (True, "")
  else (False, "expected:\n" ++ exp ++ "\nwas:\n" ++ act)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  PreprocessorTests
-- Maintainer  :  Wonhyuk Choi
module PreprocessorTests
  ( tests,
  )
where

-----------------------------------------------------------------------------

import Control.Monad (liftM)
import Data.Either (isRight)
import Distribution.TestSuite
  ( Progress (..),
    Result (..),
    Test (..),
    TestInstance (..),
  )
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath (combine)
import TSL (preprocess)
import Test.HUnit ((@=?))
import qualified Test.HUnit as H

-----------------------------------------------------------------------------

convert2Cabal :: String -> IO H.Test -> Test
convert2Cabal name = Test . testInstance name

testInstance :: String -> IO H.Test -> TestInstance
testInstance name test =
  TestInstance
    { run = runTest test,
      name = name,
      tags = [],
      options = [],
      setOption = \_ _ -> Right $ testInstance name test
    }

runTest :: IO H.Test -> IO Progress
runTest = (fmap snd . H.performTest onStart onError onFailure us =<<)
  where
    onStart :: H.State -> Progress -> IO Progress
    onStart _ = return

    onError :: a -> String -> H.State -> Progress -> IO Progress
    onError _ msg _ _ =
      return $ Finished (Error $ concat $ map (++ " ") (lines msg))

    onFailure :: a -> String -> H.State -> Progress -> IO Progress
    onFailure _ msg _ _ =
      return $ Finished (Fail $ concat $ map (++ " ") (lines msg))

    us :: Progress
    us = Finished Pass

unitTests :: [Test]
unitTests = map runTest testNums
  where
    dir :: FilePath
    dir = "src/test/regression/Preprocess/"

    testNums :: [Int]
    testNums = [0 .. 9]

    mkInputPath :: Int -> FilePath
    mkInputPath i = combine dir $ combine (show i) "input.tsl"

    mkExpectedPath :: Int -> FilePath
    mkExpectedPath i = combine dir $ combine (show i) "expected.tsl"

    runTest :: Int -> Test
    runTest testNum =
      let testName = "Preprocess Unit Test #" ++ show testNum
          inputPath = mkInputPath testNum
          expectedPath = mkExpectedPath testNum
       in convert2Cabal testName $ do
            inputTSL <- readFile inputPath
            expectedTSL <- readFile expectedPath
            case preprocess inputTSL of
              Left err -> die $ show err
              Right actual -> case preprocess expectedTSL of
                Left err -> die $ show err
                Right expected -> return $ H.TestCase $ expected @=? actual

specTests :: IO [Test]
specTests = liftM (map runTest) filePaths
  where
    dir :: FilePath
    dir = "src/test/res/specs/"

    fileNames :: IO [FilePath]
    fileNames = listDirectory dir

    validSpec :: FilePath -> Bool
    validSpec =
      let ignoreList = ["Caching.tsl", "NoteButton.tsl"]
       in not . (flip elem) ignoreList

    filePaths :: IO [FilePath]
    filePaths = do
      fileNames' <- fileNames
      return $ map (combine dir) $ filter validSpec fileNames'

    runTest :: FilePath -> Test
    runTest path = convert2Cabal ("Preprocess Spec Test: " ++ path) $ do
      tsl <- readFile path
      let parseSuccess = isRight $ preprocess tsl
      return $ H.TestCase $ H.assertBool "Preprocess failed!" parseSuccess

tests :: IO [Test]
tests = liftM (unitTests ++) specTests

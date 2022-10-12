----------------------------------------------------------------------------
-- |
-- Module      :  PreprocessorTests
-- Maintainer  :  Wonhyuk Choi
--
-- Splitting Test cases.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module PreprocessorTests
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( Progress(..)
  , Result(..)
  , Test(..)
  , TestInstance(..)
  )

import TSL (Specification, fromTSL, split, toTSL)

import Data.Char (isSpace)

import Data.Set as Set (fromList, (\\))

-----------------------------------------------------------------------------

tests :: [Test]
tests =
  let test =
        TestInstance
          { run = do
              let dirPath = "src/test/res/specs"
              paths <- listDirectory dirPath
              specs <- forM paths (\path -> loadTSL $ Just $ joinPath [dirPath, path])
              let out = show $ specifications2dependencies specs
              let refPath = "src/test/res/DepTestReference.txt"
              ref <- readFile refPath
              if out == ref
              then return $ Finished Pass
              else do
                putStrLn "Reference:"
                putStrLn ref
                putStrLn ""
                putStrLn "Generated:"
                putStrLn out
                putStrLn ""
                return $ Finished $ Fail "output differs"
          , name = "PreprocessorTest"
          , tags = []
          , options = []
          , setOption = \_ _ -> Right test
          }
  in
  map Test [test]

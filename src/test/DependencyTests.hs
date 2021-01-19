----------------------------------------------------------------------------
-- |
-- Module      :  DependencyTests
-- Maintainer  :  Marvin Stenger
--
-- Dependency test cases.
--
-----------------------------------------------------------------------------

module DependencyTests
  ( tests
  ) where

-----------------------------------------------------------------------------

import Distribution.TestSuite
  ( Progress(..)
  , Result(..)
  , Test(..)
  , TestInstance(..)
  )

-----------------------------------------------------------------------------

import TSL (specifications2dependencies)

import FileUtils (loadTSL)

import System.Directory (listDirectory)
import System.FilePath (joinPath)
import System.IO (readFile)

import Control.Monad (forM)

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
          , name = "DepTest"
          , tags = []
          , options = []
          , setOption = \_ _ -> Right test
          }
  in
  map Test [test]

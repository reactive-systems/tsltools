----------------------------------------------------------------------------
-- |
-- Module      :  ModuloTheoriesTests
-- Maintainer  :  Wonhyuk Choi
--
-- Test for Modulo Theories work.
-- These tests are meant for regression.
-- There are four types of tests corresponding to each flag of tslmt2tsl,
-- each with increasing amount of complexity:
--
-- * Predicates
-- * Context-Free Grammar
-- * Consistency Checking
-- * Syntax-Guided Synthesis
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module ModuloTheoriesTests
  ( tests
  ) where

-----------------------------------------------------------------------------

import qualified Test.HUnit as H

import Control.Monad.Trans.Except

import Distribution.TestSuite
  ( Progress(..)
  , Test(..)
  , TestInstance(..)
  , Result (..)
  )

import Test.HUnit ((@=?))

import TSL ( Error
           , TheoryPredicate
           , Cfg
           , cfgFromSpec
           , predsFromSpec
           , generateConsistencyAssumptions
           , generateSygusAssumptions
           , buildDtoList
           )

import FileUtils (writeContent, loadTSLMT, tryReadContent)


-----------------------------------------------------------------------------

convert2Cabal :: String -> H.Test -> Test
convert2Cabal name = Test . testInstance name

testInstance :: String -> H.Test -> TestInstance
testInstance name test = TestInstance
    { run       = runTest test
    , name      = name
    , tags      = []
    , options   = []
    , setOption = \_ _ -> Right $ testInstance name test
    }

runTest :: H.Test -> IO Progress
runTest = fmap snd . H.performTest onStart onError onFailure (Finished Pass)

  where

   onStart :: H.State -> Progress -> IO Progress
   onStart _ = return

   onError :: a -> String -> H.State -> Progress -> IO Progress
   onError _ msg _ _ = return $ Finished (Error $ concat $ map (++ " ") (lines msg))

   onFailure :: a -> String -> H.State -> Progress -> IO Progress
   onFailure _ msg _ _ = return $ Finished (Fail $ concat $ map (++ " ") (lines msg))

sanityTest :: H.Test
sanityTest = H.TestCase $ 47 @=? 0x2f

tests :: [Test]
tests = [convert2Cabal "Sanity Test" sanityTest]

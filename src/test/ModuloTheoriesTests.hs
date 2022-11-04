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

import qualified Data.Map as Map

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
           , Cfg (..)
           , cfgFromSpec
           , predsFromSpec
           , generateConsistencyAssumptions
           , generateSygusAssumptions
           , buildDtoList
           )

import FileUtils (writeContent, loadTSLMT, tryReadContent)

-----------------------------------------------------------------------------

convert2Cabal :: String -> IO H.Test -> Test
convert2Cabal name = Test . testInstance name

testInstance :: String -> IO H.Test -> TestInstance
testInstance name test = TestInstance
    { run       = runTest test
    , name      = name
    , tags      = []
    , options   = []
    , setOption = \_ _ -> Right $ testInstance name test
    }

runTest :: IO H.Test -> IO Progress
runTest = (fmap snd . H.performTest onStart onError onFailure us =<<)
  where
   onStart :: H.State -> Progress -> IO Progress
   onStart _ = return

   onError :: a -> String -> H.State -> Progress -> IO Progress
   onError _ msg _ _ = return $ Finished (Error $ concat $ map (++ " ") (lines msg))

   onFailure :: a -> String -> H.State -> Progress -> IO Progress
   onFailure _ msg _ _ = return $ Finished (Fail $ concat $ map (++ " ") (lines msg))

   us :: Progress
   us = Finished Pass

makeTestName :: String -> String
makeTestName = ("Modulo Theories >> " ++ )

predicatesTests :: [Test]
predicatesTests = [convert2Cabal (makeTestName "Predicates") predicateHUnit]
  where
    path = "src/test/regression/ModuloTheories/functions_and_preds.tslmt"
    expectedNumPreds = 2

    predicateHUnit = do
      (theory, spec, _)  <- loadTSLMT $ Just path
      return $ H.TestCase $ case predsFromSpec theory spec of
        Right preds -> expectedNumPreds @=? length preds
        Left errMsg -> H.assertFailure $ show errMsg

cfgTests :: [Test]
cfgTests = [convert2Cabal (makeTestName "CFG") cfgHUnit]
  where
    path = "src/test/regression/ModuloTheories/functions_and_preds.tslmt"
    expectedCfgSize = 1
    expectedProductionRuleSize = 1

    cfgHUnit = do
      (theory, spec, _)  <- loadTSLMT $ Just path
      return $ case cfgFromSpec theory spec of
        Right cfg   ->
          let assocs     = Map.assocs $ grammar cfg
              (_, rules) = head assocs 
          in H.TestList [ H.TestCase $ expectedCfgSize @=? length assocs
                        , H.TestCase $ expectedProductionRuleSize @=? length rules
                        ]
        Left errMsg -> H.TestCase $ H.assertFailure $ show errMsg

-- w :
--         (h x y (f a b) z (g a b))

tests :: [Test]
tests = concat [predicatesTests, cfgTests]

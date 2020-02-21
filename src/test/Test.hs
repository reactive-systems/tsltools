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
  ( propInverseEscapeDescape
  , propReadInput
  , propReadOutput
  )

import Test.QuickCheck
  ( Result(..)
  , quickCheckResult
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return
  [ Test qc01
  , Test qc02
  , Test qc03
  ]

  where
    qc01 = TestInstance
      { run = do
          putStrLn ""
          quickCheckResult propInverseEscapeDescape >>= \case
            Success{..} -> return $ Finished Pass
            x           -> return $ Finished $ Fail $ show x
      , name = "QuickCheck: Escape <-> Descape"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right qc01
      }

    qc02 = TestInstance
      { run = do
          putStrLn ""          
          quickCheckResult propReadInput >>= \case
            Success{..} -> return $ Finished Pass
            x           -> return $ Finished $ Fail $ show x
      , name = "QuickCheck: Read Input"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right qc02
      }

    qc03 = TestInstance
      { run = do
          putStrLn ""          
          quickCheckResult propReadOutput >>= \case
            Success{..} -> return $ Finished Pass
            x           -> return $ Finished $ Fail $ show x
      , name = "QuickCheck: Read Output"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right qc03
      }

-----------------------------------------------------------------------------

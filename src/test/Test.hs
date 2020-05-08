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
  , quickCheckResult
  )

import Control.Exception
  ( assert
  )

import Data.Char
  ( isSpace
  )
-----------------------------------------------------------------------------

tests
  :: IO [Test]

tests = return
  [ Test qc01
  , Test qc02
  , Test qc03
  , Test split01
  , Test split02
  , Test split03
  , Test split04
  , Test split05
  , Test split06
  , Test split07
  , Test split08
  , Test split09
  , Test split10
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

    split01 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_01.tsl" ["test/test_01A.tsl", "test/test_01B.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test01"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split01
      }

    split02 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_02.tsl" ["test/test_02A.tsl",
                    "test/test_02B.tsl", "test/test_02C.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test02"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split02
      }   

    split03 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_03.tsl" ["test/test_03A.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test03"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split03
      }

    split04 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_04.tsl" ["test/test_04A.tsl",
                    "test/test_04B.tsl", "test/test_04C.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test04"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split04
      }   

    split05 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_05.tsl" ["test/test_05A.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test05"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split05
      }

    split06 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_06.tsl" ["test/test_06A.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test06"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split06
      }

    split07 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_07.tsl" ["test/test_07A.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test07"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split07
      }

    split08 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_08.tsl" ["test/test_08A.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test08"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split08
      }
    
    split09 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_09.tsl" ["test/test_09A.tsl", "test/test_09B.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test09"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split09
      }

    split10 = TestInstance
      { run = do
          putStrLn ""          
          splitTest "test/test_10.tsl" ["test/test_10A.tsl", "test/test_10B.tsl"] >>= \case
            (True, _)   -> return $ Finished Pass
            (False, x)  -> do
                        putStrLn x
                        return $ Finished $ Fail "split did not return the expected result"
      , name = "SplitTest: test10"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right split10
      }

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

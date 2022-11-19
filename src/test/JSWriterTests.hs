----------------------------------------------------------------------------
-- |
-- Module      :  JSWriterTests
-- Maintainer  :  Mark Santolucito
--
-- JS Code gen tests.
-- check that the generated js code is syntactically valid 
-- note: this generates the js code from the .hoa files rather than the .tsl spec, 
-- and as such does not check for any semantic correctness
--
-----------------------------------------------------------------------------

module JSWriterTests
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

import System.Directory (listDirectory, findExecutable)
import System.FilePath (joinPath)
import System.IO (readFile)

import TSL (CodeTarget(..), implementHoa)
import Hanoi (parse)
import System.Process
import System.Exit

import Data.Maybe
-----------------------------------------------------------------------------

tests :: IO [Test]
tests =
  let jsTestTemplate dirPath hoaFile =
        TestInstance
          { run = jsCode dirPath hoaFile
          , name = "JS Code Gen: " ++ hoaFile
          , tags = []
          , options = []
          , setOption = \_ _ -> Right $ jsTestTemplate dirPath hoaFile
          }
      jsCode dirPath hoaFilepath = do
         c <- readFile $ joinPath [dirPath, hoaFilepath]
         let hoa = parse c
         let code = either id (implementHoa JS) hoa
         (exitCode, stdout, _) <- readProcessWithExitCode "jshint" ["/dev/stdin"] code
         return $ case exitCode of
           ExitSuccess -> Finished Pass
           ExitFailure _ -> 
             Finished $ Fail stdout
  in do
    let dirPath = "src/test/res/hoa"
    paths <- listDirectory dirPath
    jsHintPath <- findExecutable "jshint"
    if isJust jsHintPath
    then return $ map Test $ map (jsTestTemplate dirPath) paths
    else return []



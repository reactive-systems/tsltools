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

import           Distribution.TestSuite         ( Progress(..)
                                                , Result(..)
                                                , Test(..)
                                                , TestInstance(..)
                                                )

-----------------------------------------------------------------------------

import           System.Directory               ( findExecutable
                                                , listDirectory
                                                )
import           System.FilePath                ( joinPath )
import           System.IO                      ( readFile )

import           Hanoi                          ( parse )
import           System.Exit
import           System.Process
import           TSL                            ( CodeTarget(..)
                                                , implementHoa
                                                )

import           Data.Maybe
-----------------------------------------------------------------------------

tests :: IO [Test]
tests =
  let jsTestTemplate dirPath hoaFile = TestInstance
        { run       = jsCode dirPath hoaFile
        , name      = "JS Code Gen: " ++ hoaFile
        , tags      = []
        , options   = []
        , setOption = \_ _ -> Right $ jsTestTemplate dirPath hoaFile
        }
      jsCode dirPath hoaFilepath = do
        c <- readFile $ joinPath [dirPath, hoaFilepath]
        let hoa  = parse c
        let code = either id (implementHoa JS) hoa
        (exitCode, stdout, stderr) <- readProcessWithExitCode
          "jshint"
          ["--config=./.jshintrc", "/dev/stdin"]
          code
        case exitCode of
          ExitSuccess   -> return $ Finished Pass
          ExitFailure _ -> do
            putStrLn $ "\nSynthesized JS code:\n" ++ code ++ "\n"
            return $ Finished $ Fail (stdout ++ stderr)
  in  do
        let dirPath = "src/test/res/hoa"
        paths      <- listDirectory dirPath
        jsHintPath <- findExecutable "jshint"
        if isJust jsHintPath
          then return $ map Test $ map (jsTestTemplate dirPath) paths
          else return []
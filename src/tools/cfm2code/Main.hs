----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Generates code from a TSL control flow model.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config
  ( Configuration(..)
  , parseArguments
  )

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  , putErr
  , putErrLn
  )

import TSL
  ( fromCFM
  , implement
  )

import Control.Exception
  ( assert
  )

import Control.Monad
  ( when
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitSuccess
  , exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{..} <- getArgs >>= parseArguments

  when pHelp $ do
    cPutOutLn Dull White   ""
    cPutOut Vivid Yellow   "Usage:"
    cPutOut Vivid White    " cfm2code [OPTIONS]"
    cPutOut Vivid Blue     " <target>"
    cPutOutLn Dull White   " <file>"
    cPutOutLn Dull White   ""
    cPutOutLn Dull White   "  Generates code from a TSL control flow model."
    cPutOutLn Dull White   ""
    cPutOutLn Vivid Yellow "Options:"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -o, --output"
    cPutOutLn Dull White   " <file>            path of the output file (results are"
    cPutOutLn Dull White   "                                 printed to STDOUT if not set)"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -m, --module-name"
    cPutOutLn Dull White   " <string>     overwrites the name of the generated"
    cPutOutLn Dull White   "                                 module; if not set, the filename of the"
    cPutOutLn Dull White   "                                 passed input file is used; if reading"
    cPutOutLn Dull White   "                                 from STDIN, the default 'Synth' is used"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -f, --function-name"
    cPutOutLn Dull White   " <string>   overwrites the name of the exported"
    cPutOutLn Dull White   "                                 function; if not set, the filename of the"
    cPutOutLn Dull White   "                                 passed input file is used; if reading"
    cPutOutLn Dull White   "                                 from STDIN, the default 'cfm' is used"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -h, --help"
    cPutOutLn Dull White   "                     display this help"
    cPutOutLn Dull White   ""
    cPutOutLn Vivid Yellow "Targets:"
    cPutOutLn Dull White   ""
    cPutOut Vivid Blue     "  applicative"
    cPutOutLn Dull White   "                    generates code for 'Applicative'-FRP libraries"
    cPutOut Vivid Blue     "  monadic"
    cPutOutLn Dull White   "                        generates code for 'Monadic'-FRP libraries"
    cPutOut Vivid Blue     "  arrow"
    cPutOutLn Dull White   "                          generates code for 'Arrowized'-FRP libraries"
    cPutOut Vivid Blue     "  clash"
    cPutOutLn Dull White   "                          generates code for the hardware desciption"
    cPutOutLn Dull White   "                                 language 'ClaSH'"
    cPutOutLn Dull White   ""
    cPutOutLn Dull White   "If no input file is given, the input is read from STDIN."
    cPutOutLn Dull White   ""

    exitSuccess

  cnt <- case inputFile of
    Nothing   -> getContents
    Just file -> readFile file

  case fromCFM cnt of
    Left err -> do
      putErrLn err
      exitFailure
    Right cfm -> case codeTarget of
      Nothing -> assert False undefined -- TODO: check this earlier
      Just t  ->
        let code = implement t moduleName functionName cfm
        in case outputFile of
          Nothing   -> putStrLn code
          Just file -> writeFile file code

  exitSuccess

-----------------------------------------------------------------------------

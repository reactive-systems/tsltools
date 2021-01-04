----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- This module implements different utilities that can be used in the
-- different core genearation tools
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, ImplicitParams #-}

-----------------------------------------------------------------------------
module Utils where

import PrintUtils
  ( putOut
  , putOutLn
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSLCoreGenerator (Context(..), Verbosity(..))

import TSL (Specification, fromTSL, toTLSF)

import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.Process (readProcessWithExitCode)

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hSetSGR
  )

import System.IO
  ( BufferMode(..)
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )

import Data.Foldable (traverse_)

import Text.Read (readMaybe)

-----------------------------------------------------------------------------
-- | Generates a IO check given a CMD and am output classification
--
command :: String -> (String -> Maybe Bool) -> String -> IO Bool
command cmd result tlsf = do
  (out, err) <- execCMD cmd tlsf
  case result out of
    Just b -> return b
    Nothing -> do
      cPutErrLn Vivid Red $ "Command execution \"" ++ cmd ++ "\"failed with: "
      cPutErrLn Vivid White $ "TLSF: " ++ tlsf
      cPutErrLn Vivid White $ "StdOut: " ++ show out
      cPutErrLn Vivid White $ "StdErr: " ++ show err
      exitFailure

-----------------------------------------------------------------------------
-- | Executes a given command with an input given to stdin and return 
-- stdout and stderr
--
execCMD :: String -> String -> IO (String, String)
execCMD cmd stdIn = do
  (_, sout, serr) <- readProcessWithExitCode cmd [] stdIn
  return (sout, serr)

-----------------------------------------------------------------------------
-- | Creates out of the CMD interface an encapuled tool call interface. 
-- Therefore TSL to string methods are used
--
createContext :: Int -> Verbosity -> String -> Context
createContext poolSize verbosity realCmd =
  let realCall =
        command
          realCmd
          (\case
             "REALIZABLE" -> Just True
             "REALIZABLE\n" -> Just True
             "UNREALIZABLE" -> Just False
             "UNREALIZABLE\n" -> Just False
             _ -> Nothing)
   in Context
        { tslSpecRealizable = realCall . toTLSF "Specification"
        , verbosityLevel = verbosity
        , threadPoolSize = poolSize
        }

-----------------------------------------------------------------------------
-- | 'printHelpAndExit' prints a help message in an adequate format and exits
-- afterwards with a failure
printHelpAndExit :: [String] -> IO a
printHelpAndExit helpMessages = do
  cPutErr Vivid Yellow "Usage: "
  traverse_ (cPutErrLn Vivid White) helpMessages
  exitFailure

-----------------------------------------------------------------------------
-- | 'parsePoolSize' tries to parse the pool size and if this is not
-- possble outputs an aquedate error message on stderr and exists the program
parsePoolSize :: String -> IO Int
parsePoolSize poolSizeStr =
  case readMaybe poolSizeStr :: Maybe Int of
    Just n ->
      if n <= 0
        then do
          cPutErrLn Vivid Red "The thread pool size has to be at least one"
          exitFailure
        else return n
    Nothing -> do
      cPutErrLn Vivid Red "The thread pool size has to be a (natural) number"
      exitFailure

-----------------------------------------------------------------------------
-- | 'parseVerbosity' tries to parse a verbosity and if this is not
-- possble outputs an aquedate error message on stderr and exists the program
parseVerbosity :: String -> IO Verbosity
parseVerbosity string =
  case readMaybe string :: Maybe Int of
    Just 0 -> return SILENT
    Just 1 -> return QUIET
    Just 2 -> return STEPWISE
    Just 3 -> return DETAILED
    _ -> do
      cPutErrLn
        Vivid Red
        "The verbosity has to be given by a number between zero and three"
      exitFailure
-----------------------------------------------------------------------------

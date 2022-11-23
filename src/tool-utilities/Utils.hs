----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Utils
-- Maintainer  :  Philippe Heim
--
-- This module implements different utilities that can be used in the
-- different core genearation tools
module Utils where

import Control.Monad (when)
import Data.Foldable (traverse_)
import PrintUtils (Color (..), ColorIntensity (..), cPutErr, cPutErrLn)
import System.Exit (exitFailure)
import System.Process (readProcessWithExitCode)
import TSL (toTLSF)
import TSLCoreGenerator (Context (..), Verbosity (..))
import Text.Read (readMaybe)

-----------------------------------------------------------------------------

-- | Generates a IO check given a CMD and am output classification
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
execCMD :: String -> String -> IO (String, String)
execCMD cmd stdIn = do
  (_, sout, serr) <- readProcessWithExitCode cmd [] stdIn
  return (sout, serr)

-----------------------------------------------------------------------------

-- | Creates out of the CMD interface an encapuled tool call interface.
-- Therefore TSL to string methods are used
createContext :: Int -> Verbosity -> String -> Context
createContext poolSize verbosity realCmd =
  let realCall =
        command
          realCmd
          ( \case
              "REALIZABLE" -> Just True
              "REALIZABLE\n" -> Just True
              "UNREALIZABLE" -> Just False
              "UNREALIZABLE\n" -> Just False
              _ -> Nothing
          )
   in Context
        { tslSpecRealizable = realCall . toTLSF "Specification",
          verbosityLevel = verbosity,
          threadPoolSize = poolSize
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

-- | 'checkPoolSize' checks the pool size and if this is invalid
-- outputs an adequate error message on stderr and exists the program
checkPoolSize :: Int -> IO ()
checkPoolSize n =
  when (n <= 0) $ do
    cPutErrLn Vivid Red "The thread pool size has to be at least one"
    exitFailure

-----------------------------------------------------------------------------

-- | 'parsePoolSize' tries to parse the pool size and if this is not
-- possible outputs an adequate error message on stderr and exists the program
parsePoolSize :: String -> IO Int
parsePoolSize poolSizeStr =
  case readMaybe poolSizeStr :: Maybe Int of
    Just n -> do
      checkPoolSize n
      return n
    Nothing -> do
      cPutErrLn Vivid Red "The thread pool size has to be a (natural) number"
      exitFailure

-----------------------------------------------------------------------------

-- | 'convertVerbosity' tries to convert a verbosity and if this is not
-- possible outputs an adequate error message on stderr and exits the program
convertVerbosity :: Int -> IO Verbosity
convertVerbosity v =
  case v of
    0 -> return SILENT
    1 -> return QUIET
    2 -> return STEPWISE
    3 -> return DETAILED
    _ -> do
      cPutErrLn
        Vivid
        Red
        "The verbosity has to be given by a number between zero and three"
      exitFailure

-----------------------------------------------------------------------------

-- | 'parseVerbosity' tries to parse a verbosity and if this is not
-- possible outputs an adequate error message on stderr and exits the program
parseVerbosity :: String -> IO Verbosity
parseVerbosity string =
  case readMaybe string :: Maybe Int of
    Just n -> convertVerbosity n
    _ -> do
      cPutErrLn
        Vivid
        Red
        "The verbosity has to be given by a number between zero and three"
      exitFailure

-----------------------------------------------------------------------------

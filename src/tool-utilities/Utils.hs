----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- This module implements differnt utilities that can are used in the 
-- different core genearation tools
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, ImplicitParams #-}

-----------------------------------------------------------------------------
module Utils where

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
  , setSGR
  )
import System.IO
  ( BufferMode(..)
  , hPrint
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )

import Data.Foldable (traverse_)
import GHC.IO.Encoding
  ( setFileSystemEncoding
  , setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

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
      cErrorLn Red $ "Command execution \"" ++ cmd ++ "\"failed with: "
      cErrorLn White $ "TLSF: " ++ tlsf
      cErrorLn White $ "StdOut: " ++ show out
      cErrorLn White $ "StdErr: " ++ show err
      resetColors
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
-- | 'cPutStr' writes a colored message on stdout
cPutStr :: Color -> String -> IO ()
cPutStr c str = do
  setSGR [SetColor Foreground Vivid c]
  putStr str

-----------------------------------------------------------------------------
-- | 'cPutStrLn' writes a colored message on stdout and adds a newline at
-- the end
cPutStrLn :: Color -> String -> IO ()
cPutStrLn c str = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn str

-----------------------------------------------------------------------------
-- | 'cError' writes a colored error message on stderr
cError :: Color -> String -> IO ()
cError c str = do
  hSetSGR stderr [SetColor Foreground Vivid c]
  hPutStr stderr str

-----------------------------------------------------------------------------
-- | 'cErrorLn' writes a colored error message on stderr and adds a new line
-- at the end
cErrorLn :: Color -> String -> IO ()
cErrorLn c str = do
  hSetSGR stderr [SetColor Foreground Vivid c]
  hPutStrLn stderr str

-----------------------------------------------------------------------------
-- | 'resteColors' reset the colors on the terminal and should be called 
-- after using 'cPutStr', 'cPutStrLn', 'cError' or 'cErrorLn'.
resetColors :: IO ()
resetColors = do
  hSetSGR stderr [Reset]
  setSGR [Reset]

-----------------------------------------------------------------------------
-- | 'tryLoadTSL' is a helper function which load and parses a TSL file and
-- if this is not possible outputs a respective error on the command line
-- and exists
tryLoadTSL :: FilePath -> IO Specification
tryLoadTSL filepath = do
  exists <- doesFileExist filepath
  if not exists
    then do
      cError Red "File not found: "
      cErrorLn White filepath
      resetColors
      exitFailure
    else do
      str <- readFile filepath
      let ?specFilePath = Just filepath
      tsl <- fromTSL str
      case tsl of
        Left err -> do
          cPutStr Red "invalid: "
          cPutStrLn White filepath
          resetColors
          hPrint stderr err
          exitFailure
        Right spec -> return spec

-----------------------------------------------------------------------------
-- | 'printHelpAndExit' prints a help message in an aquedate format and exits
-- afterwards with a failure
printHelpAndExit :: [String] -> IO a
printHelpAndExit helpMessages = do
  cError Yellow "Usage: "
  traverse_ (cErrorLn White) helpMessages
  resetColors
  exitFailure

-----------------------------------------------------------------------------
-- | 'initEncoding' initializes the standard encoding
initEncoding :: IO ()
initEncoding = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

-----------------------------------------------------------------------------
-- | 'parsePoolSize' tries to parse the pool size and if this is not
-- possble outputs an aquedate error message on stderr and exists the program
parsePoolSize :: String -> IO Int
parsePoolSize poolSizeStr =
  case readMaybe poolSizeStr :: Maybe Int of
    Just n ->
      if n <= 0
        then do
          cErrorLn Red "The thread pool size has to be at least one"
          resetColors
          exitFailure
        else return n
    Nothing -> do
      cErrorLn Red "The thread pool size has to be a (natural) number"
      resetColors
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
      cErrorLn
        Red
        "The verbosity has to be given by a number between zero and three"
      resetColors
      exitFailure
-----------------------------------------------------------------------------

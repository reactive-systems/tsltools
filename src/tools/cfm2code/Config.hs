-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Configuration of the tool, set up via the command line arguments.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , parseArguments
  ) where

-----------------------------------------------------------------------------

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( CodeTarget(..)
  )

import Data.Char
  ( toLower
  , toUpper
  )

import System.Exit
  ( exitFailure
  )

import Control.Monad
  ( unless
  )

import System.Directory
  ( doesFileExist
  )

import System.FilePath
  ( takeBaseName
  )

-----------------------------------------------------------------------------

-- | Supported library exportation formats

data Args a = None a | Single a

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:

data Configuration =
  Configuration
  { -- | The input file containing the synthesized control flow
    -- model. If no input file is given we read from STDIN.
    inputFile :: Maybe FilePath
  , -- | Output file path. If no path is given, the output is writtend
    -- to STDOUT.
    outputFile :: Maybe FilePath
  , -- | Target code type to generate.
    codeTarget :: Maybe CodeTarget
  , -- | The name of the generated module.
    moduleName :: String
  , -- | Name of the synthesized signal function that is exported by
    -- the module.
    functionName :: String
    -- | A boolean flag specifying whether the help info should be
    -- printed or not.
  , pHelp :: Bool
  } deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- ^ Default configuration.

defaultCfg
  :: Configuration

defaultCfg =
  Configuration
    { inputFile    = Nothing
    , outputFile   = Nothing
    , codeTarget   = Nothing
    , moduleName   = ""
    , functionName = ""
    , pHelp        = False
    }

-----------------------------------------------------------------------------

parseArguments
  :: [String] -> IO Configuration

parseArguments args = do
  c <- traverse defaultCfg args
  case codeTarget c of
    Nothing
      | pHelp c   -> return c
      | otherwise -> do
          cPutErr Vivid Red    "Missing target:"
          cPutErr Dull White   " Use"
          cPutErr Dull Yellow  " --help"
          cPutErrLn Dull White " to see supported targets."
          exitFailure

    Just _  ->
      return
        c { moduleName = fstUpper $ case moduleName c of
              "" -> case inputFile c of
                Just file -> takeBaseName file
                Nothing   -> "Synth"
              x  -> x
          , functionName = fstLower $ case functionName c of
               "" -> case inputFile c of
                 Just file -> takeBaseName file
                 Nothing   -> "cfm"
               x  -> x
          }

  where
    traverse c = \case
      x:y:xr ->
        parseArgument c (Just y) x >>= \case
          Single c' -> traverse c' xr
          None c'   -> traverse c' (y:xr)
      [x]    ->
        parseArgument c Nothing x >>= \case
          Single c' -> return c'
          None c'   -> return c'
      []     ->
        return c

    parseArgument c next = \case
      "-o"            -> case next of
        Just file  -> return $ Single $ c { outputFile = Just file }
        Nothing    -> argsError "-o" "No output file given."

      "--output"      -> case next of
        Just _  -> parseArgument c next "-o"
        Nothing -> argsError "--output" "No output file given."

      "-m"            -> case next of
        Just name -> return $ Single $ c { moduleName = name }
        Nothing   -> argsError "-m" "No module name given."

      "--module-name" -> case next of
        Just _  -> parseArgument c next "-m"
        Nothing -> argsError "--module-name" "No module name given."

      "-f"            ->case next of
        Just name -> return $ Single $ c { functionName = name }
        Nothing   -> argsError "-m" "No function export name given."

      "--function-name" -> case next of
        Just _  -> parseArgument c next "-f"
        Nothing -> argsError "--function-name" "No function export name given."

      "-h"          -> simple $ defaultCfg { pHelp = True }

      "--help"      -> parseArgument c next "-h"

      "applicative" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just Applicative }
        Just t  ->
          argsError
            ("applicative vs. " ++ prTarget t)
            "The specified target must be unique."

      "monadic" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just Monadic }
        Just t  ->
          argsError
            ("monadic vs. " ++ prTarget t)
            "The specified target must be unique."

      "arrow" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just Arrow }
        Just t  ->
          argsError
            ("arrow vs. " ++ prTarget t)
            "The specified target must be unique."

      "clash" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just Clash }
        Just t  ->
          argsError
            ("clash vs. " ++ prTarget t)
            "The specified target must be unique."

      file           -> do
        exists <- doesFileExist file
        unless exists $ argsError "File not found" file
        simple $ c { inputFile = Just file  }

    prTarget = \case
      Applicative -> "applicative"
      Arrow       -> "arrow"
      Clash       -> "clash"
      Monadic     -> "monadic"

    fstLower = \case
      []   -> []
      x:xr -> toLower x : xr

    fstUpper = \case
      []   -> []
      x:xr -> toUpper x : xr

    argsError h str = do
      unless (null h) $
        cPutErr Vivid Red $ h ++ ": "
      cPutErrLn Dull White str
      exitFailure

    simple =
      return . None

-----------------------------------------------------------------------------

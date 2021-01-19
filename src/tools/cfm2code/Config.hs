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
  , cPutStr
  , cPutStrLn
  , cErrStr
  , cErrStrLn
  ) where

-----------------------------------------------------------------------------

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

import System.IO
  ( hPutStr
  , hPutStrLn
  , stderr
  )

import System.Directory
  ( doesFileExist
  )

import System.FilePath
  ( takeBaseName
  )

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , hSetSGR
  , setSGR
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
          cErrStr Vivid Red    "Missing target:"
          cErrStr Dull White   " Use"
          cErrStr Dull Yellow  " --help"
          cErrStrLn Dull White " to see supported targets."
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

      "JavaScript" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just JavaScript }
        Just t  ->
          argsError
            ("JavaScript vs. " ++ prTarget t)
            "The specified target must be unique."

      "WebAudio" -> case codeTarget c of
        Nothing -> simple $ c { codeTarget = Just WebAudio }
        Just t  ->
          argsError
            ("WebAudio vs. " ++ prTarget t)
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
      JavaScript  -> "JavaScript"
      WebAudio    -> "WebAudio"

    fstLower = \case
      []   -> []
      x:xr -> toLower x : xr

    fstUpper = \case
      []   -> []
      x:xr -> toUpper x : xr

    argsError h str = do
      unless (null h) $
        cErrStr Vivid Red $ h ++ ": "
      cErrStrLn Dull White str
      exitFailure

    simple =
      return . None

-----------------------------------------------------------------------------

cPutStr
  :: ColorIntensity -> Color -> String -> IO ()

cPutStr v c str = do
  setSGR [ SetColor Foreground v c ]
  putStr str


-----------------------------------------------------------------------------

cPutStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cPutStrLn v c str = do
  setSGR [ SetColor Foreground v c ]
  putStrLn str

-----------------------------------------------------------------------------

cErrStr
  :: ColorIntensity -> Color -> String -> IO ()

cErrStr v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStr stderr str

-----------------------------------------------------------------------------

cErrStrLn
  :: ColorIntensity -> Color -> String -> IO ()

cErrStrLn v c str = do
  hSetSGR stderr [ SetColor Foreground v c ]
  hPutStrLn stderr str

-----------------------------------------------------------------------------

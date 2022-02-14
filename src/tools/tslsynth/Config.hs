-----------------------------------------------------------------------------
-- |
-- Module      :  Config
-- Maintainer  :  Felix Klein
--
-- Configuration of the tool, set up via the command line arguments.
-- TODO move to tool-utlis
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

module Config
  ( Configuration(..)
  , parseArguments
  ) where

-----------------------------------------------------------------------------

import PrintUtils (Color(..), ColorIntensity(..), cPutErr, cPutErrLn)

import Data.Semigroup ((<>))
import Options.Applicative

import TSL (CodeTarget(..))

import Data.Char (toLower, toUpper)

import System.Exit (exitFailure)

import Control.Monad (unless)

import System.Directory (doesFileExist)

import System.FilePath (takeBaseName)

-----------------------------------------------------------------------------

-- | The data type contains all flags and settings
-- that can be adjusted to influence the behavior of the library:
data Configuration =
  Configuration
  { -- | The input file containing the synthesized control flow
    -- model. If no input file is given we read from STDIN.
    input :: Maybe FilePath
    -- | Output file path. If no path is given, the output is writtend
    -- to STDOUT.
  , output :: Maybe FilePath
    -- | Target code type to generate.
  , codeTarget :: CodeTarget
    -- | The name of the generated module.
  , moduleName :: String
    -- | Name of the synthesized signal function that is exported by
    -- the module.
  , functionName :: String
  , writeHoa :: FilePath
  } deriving (Eq, Ord)

configParser :: Parser Configuration
configParser = Configuration
  <$> optional (argument str
        (  metavar "INFILE"
        <> help "input file (STDIN, if not set)"
        )
      )
  <*> optional (option str
        (  long "output"
        <> short 'o'
        <> metavar "OUTFILE"
        <> help "output file (STDOUT, if not set)"
        )
      )
  <*> ( flag' Applicative (long "applicative" <> help "generates code for 'Applicative'-FRP libraries")
    <|> flag' Monadic     (long "monadic"     <> help "generates code for 'Monadic'-FRP libraries")
    <|> flag' Arrow       (long "arrow"       <> help "generates code for 'Arrowized'-FRP libraries")
    <|> flag' Clash       (long "clash"       <> help "generates code for the hardware description language 'ClaSH'")
    <|> flag' JavaScript  (long "javascript"  <> help "generates code for Javascript")
    <|> flag' WebAudio    (long "webaudio"    <> help "generates code for JS+WebAudio backend")
    <|> flag' Python      (long "python"      <> help "generates code for Python")
    )
  <*> option str
      (  long "module-name"
      <> short 'm'
      <> help "overwrites the name of the generated module; if not set, the filename of the passed input file is used; if reading from STDIN, the default 'Synth' is used"
      <> metavar "MOD"
      <> value ""
      )
  <*> option str
      (  long "function-name"
      <> short 'f'
      <> help "overwrites the name of the exported function; if not set, the filename of the passed input file is used; if reading from STDIN, the default 'cfm' is used"
      <> metavar "FUNC"
      <> value ""
      )
  <*> option str
      (  long "write-hoa"
      <> short 'h'
      <> help "Writes the inermediate HOA file that was generated to the given filepath "
      <> metavar "HOA"
      <> value ""
      )

configParserInfo :: ParserInfo Configuration
configParserInfo = info (configParser <**> helper)
  (  fullDesc
  <> header "cfm2code - generates code from a TSL control flow model"
  )

parseArguments :: IO Configuration
parseArguments = do
  c@Configuration{input} <- customExecParser pprefs configParserInfo

  case input of
    Just file -> do
      exists <- doesFileExist file
      unless exists $ argsError "File not found" file
    Nothing -> return ()

  return
    c { moduleName = fstUpper $ case moduleName c of
          "" -> case input of
            Just file -> takeBaseName file
            Nothing   -> "Synth"
          x  -> x
      , functionName = fstLower $ case functionName c of
          "" -> case input of
            Just file -> takeBaseName file
            Nothing   -> "cfm"
          x  -> x
      }

  where
    pprefs = prefs disambiguate

    argsError h str = do
      unless (null h) $
        cPutErr Vivid Red $ h ++ ": "
      cPutErrLn Dull White str
      exitFailure

    fstLower = \case
      []   -> []
      x:xr -> toLower x : xr

    fstUpper = \case
      []   -> []
      x:xr -> toUpper x : xr

-----------------------------------------------------------------------------

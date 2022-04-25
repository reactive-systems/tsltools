----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Runs the full synthesis pipeline
--
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config (Configuration(..), parseArguments)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
 
import TSL (toTLSF, implementHoa)

import Hanoi (parse)

import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

import System.FilePath.Posix (takeBaseName)

import System.Process (readProcessWithExitCode)
import System.Exit 

import qualified Syfco as S
-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, output, codeTarget, moduleName, functionName, writeHoa} <- parseArguments
  let fileBasename = takeBaseName $ fromJust input

  -- tsl2tlsf
  spec <- loadTSL input
  let tlsfSpec = toTLSF fileBasename spec
  writeFile "test.tlsf" tlsfSpec
 
  -- call ltlSynt
  hoaOutput <- callLtlsynt tlsfSpec

  hoaContents <- 
    if isPrefixOf "REALIZABLE" hoaOutput
    then do
      return $ unlines $ tail $ lines $ hoaOutput
    else do
      print hoaOutput 
      error "unrealizable spec?"
      --error $ tslCoreGen $ fromJust input

  if writeHoa /= ""
  then writeFile writeHoa hoaContents
  else return ()

  let hoa = parse hoaContents
  putStrLn $ either id (implementHoa codeTarget) hoa

{-
TODO figure out how the realizableCommand should work
tslCoreGen :: String -> IO()
tslCoreGen spec = generateCore (createContext poolSize verbosity' realizableCommand) spec
  >>= \case
    Nothing -> error "HOA file did not start with REALIZABLE, but the TSL spec was realizable"
    Just core -> do
      putStrLn "UNREALIZABLE CORE"
      putStr $ toTSL core
-}

callLtlsynt :: String -> IO String
callLtlsynt tlsfContents = do
  let tlsfSpec = 
        case S.fromTLSF tlsfContents of
          Left err -> error $ show err
          Right spec -> spec 
  let  
    ltlIns      = prInputs S.defaultCfg tlsfSpec
    ltlOuts     = prOutputs S.defaultCfg tlsfSpec
    ltlFormulae = prFormulae S.defaultCfg{ S.outputMode = S.Fully, S.outputFormat = S.LTLXBA } tlsfSpec
    ltlCommandArgs = [ltlFormulae, ltlIns, ltlOuts]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "./tlsfSynt.sh" ltlCommandArgs []
  if exitCode /= ExitSuccess
  then
    print "TSL spec UNREALIZABLE" >>
    return "UNREALIZABLE"
  else
    return stdout


prFormulae 
  :: S.Configuration -> S.Specification -> String

prFormulae c s = case S.apply c s of
  Left err     -> show err
  Right formulae -> formulae

-----------------------------------------------------------------------------

-- | Prints the input signals of the given specification.

prInputs
  :: S.Configuration -> S.Specification -> String

prInputs c s = case S.inputs c s of
  Left err     -> show err
  Right ([])   -> ""
  Right (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.

prOutputs
  :: S.Configuration -> S.Specification -> String

prOutputs c s = case S.outputs c s of
  Left err     -> show err
  Right (x:xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr


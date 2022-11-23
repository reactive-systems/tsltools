----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Runs the full synthesis pipeline
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import Config (Configuration (..), parseArguments)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL, tryReadContent)
import Hanoi (parse)
import qualified Syfco as S
import System.Exit
import System.FilePath.Posix (takeBaseName)
import System.Process (readProcessWithExitCode)
import TSL (implementHoa, preprocess, toTLSF)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  Configuration {input, codeTarget, writeHoa} <- parseArguments
  let fileBasename = takeBaseName $ fromJust input

  -- tslmt2tsl
  content <- tryReadContent input
  case preprocess (content ++ "\n") of
    Left errMsg -> die $ show errMsg
    Right processed -> writeFile "tmp.tsl" $ show processed

  -- tsl2tlsf
  spec <- loadTSL (Just "tmp.tsl")
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
  -- error $ tslCoreGen $ fromJust input

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
  let ltlIns = prInputs S.defaultCfg tlsfSpec
      ltlOuts = prOutputs S.defaultCfg tlsfSpec
      ltlFormulae = prFormulae S.defaultCfg {S.outputMode = S.Fully, S.outputFormat = S.LTLXBA} tlsfSpec
      ltlCommandArgs = [ltlFormulae, ltlIns, ltlOuts]
  (exitCode, stdout, _) <- readProcessWithExitCode "./tlsfSynt.sh" ltlCommandArgs []
  if exitCode /= ExitSuccess
    then
      print "TSL spec UNREALIZABLE"
        >> return "UNREALIZABLE"
    else return stdout

prFormulae ::
  S.Configuration -> S.Specification -> String
prFormulae c s = case S.apply c s of
  Left err -> show err
  Right formulae -> formulae

-----------------------------------------------------------------------------

-- | Prints the input signals of the given specification.
prInputs ::
  S.Configuration -> S.Specification -> String
prInputs c s = case S.inputs c s of
  Left err -> show err
  Right ([]) -> ""
  Right (x : xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr

-----------------------------------------------------------------------------

-- | Prints the output signals of the given specification.
prOutputs ::
  S.Configuration -> S.Specification -> String
prOutputs c s = case S.outputs c s of
  Left err -> show err
  Right (x : xr) -> x ++ concatMap ((:) ',' . (:) ' ') xr
  _ -> error "Uncaught pattern match"

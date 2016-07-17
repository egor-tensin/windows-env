{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad    (when)
import Data.List        (union)
import Data.Maybe       (fromMaybe)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath  (combine)

import Options.Applicative

import qualified Environment

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> optYesDesc
    <*> optGlobalDesc
  where
    optYesDesc = switch $
        long "yes" <> short 'y' <>
        help "Skip confirmation prompt"
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Set up for all users"

data Dirs = Dirs
    { pdbsDir    :: String
    , symbolsDir :: String
    } deriving (Eq, Show)

getRemoteDirs :: Dirs -> Dirs
getRemoteDirs localDirs = localDirs
    { symbolsDir = remoteSymbolsDir $ symbolsDir localDirs
    }
  where
    remoteSymbolsDir localDir = "SRV*" ++ localDir ++ "*" ++ remoteSymbolsUrl
    remoteSymbolsUrl = "http://msdl.microsoft.com/download/symbols"

getLocalDirs :: IO Dirs
getLocalDirs = do
    cwd <- getCurrentDirectory
    return Dirs
        { pdbsDir    = combine cwd "pdbs"
        , symbolsDir = combine cwd "symbols"
        }

fixNtSymbolPath :: Options -> IO ()
fixNtSymbolPath options = do
    oldValue <- query
    let oldPaths = Environment.pathSplit $ fromMaybe "" oldValue
    localDirs <- getLocalDirs
    let remoteDirs = getRemoteDirs localDirs
    let newPaths = union oldPaths $ paths remoteDirs
    when (length oldPaths /= length newPaths) $ do
        let newValue = Environment.pathJoin newPaths
        confirmed <- engrave newValue
        when confirmed $
            createLocalDirs localDirs
  where
    varName = "_NT_SYMBOL_PATH"

    forAllUsers = optGlobal options
    env = if forAllUsers
        then Environment.AllUsers
        else Environment.CurrentUser

    query = Environment.query env varName

    skipPrompt = optYes options
    engrave value = if skipPrompt
        then Environment.engrave       env varName value >> return True
        else Environment.engravePrompt env varName value

    paths dirs = [pdbsDir dirs, symbolsDir dirs]

    createLocalDirs = mapM_ (createDirectoryIfMissing True) . paths

main :: IO ()
main = execParser parser >>= fixNtSymbolPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Set up your _NT_SYMBOL_PATH"

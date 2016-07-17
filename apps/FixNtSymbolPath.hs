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

import qualified Utils

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

dirPaths :: Dirs -> [String]
dirPaths dirs = [ pdbsDir dirs
                , symbolsDir dirs
                ]

createDirs :: Dirs -> IO ()
createDirs = mapM_ (createDirectoryIfMissing True) . dirPaths

toRemoteDirs :: Dirs -> Dirs
toRemoteDirs localDirs = localDirs
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
    oldValue <- Environment.query profile varName
    let oldPaths = Environment.pathSplit $ fromMaybe "" oldValue
    localDirs <- getLocalDirs
    let remoteDirs = toRemoteDirs localDirs
    let newPaths = union oldPaths $ dirPaths remoteDirs
    when (length oldPaths /= length newPaths) $ do
        let newValue = Environment.pathJoin newPaths
        let promptBanner = Utils.engraveBanner profile varName oldValue newValue
        confirmed <- prompt promptBanner $ Environment.engrave profile varName newValue
        when confirmed $
            createDirs localDirs
  where
    varName = "_NT_SYMBOL_PATH"

    forAllUsers = optGlobal options
    profile = if forAllUsers
        then Environment.AllUsers
        else Environment.CurrentUser

    skipPrompt = optYes options
    prompt = if skipPrompt
        then const Utils.withoutPrompt
        else Utils.withPrompt

main :: IO ()
main = execParser parser >>= fixNtSymbolPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Set up your _NT_SYMBOL_PATH"

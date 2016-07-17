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

getRemoteSymbolsDirectoryPath :: IO String
getRemoteSymbolsDirectoryPath = do
    localPath <- getLocalPath
    createDirectoryIfMissing True localPath
    return $ "SRV*" ++ localPath ++ "*" ++ remotePath
  where
    getLocalPath = do
        cwd <- getCurrentDirectory
        return $ combine cwd "symbols"
    remotePath = "http://msdl.microsoft.com/download/symbols"

getPdbsDirectoryPath :: IO String
getPdbsDirectoryPath = do
    cwd <- getCurrentDirectory
    let path = combine cwd "pdbs"
    createDirectoryIfMissing True path
    return path

fixNtSymbolPath :: Options -> IO ()
fixNtSymbolPath options = do
    oldValue <- Environment.query env varName
    let oldPaths = Environment.pathSplit $ fromMaybe "" oldValue
    pathsToAdd <- addPaths
    let newPaths = union oldPaths pathsToAdd
    when (length oldPaths /= length newPaths) $ do
        let newValue = Environment.pathJoin newPaths
        engrave env varName newValue
  where
    varName = "_NT_SYMBOL_PATH"
    addPaths = sequence [getRemoteSymbolsDirectoryPath, getPdbsDirectoryPath]

    forAllUsers = optGlobal options
    env | forAllUsers = Environment.AllUsers
        | otherwise   = Environment.CurrentUser

    skipPrompt = optYes options
    engrave
        | skipPrompt = Environment.engrave
        | otherwise  = Environment.engraveWithPrompt

main :: IO ()
main = execParser parser >>= fixNtSymbolPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Set up your _NT_SYMBOL_PATH"

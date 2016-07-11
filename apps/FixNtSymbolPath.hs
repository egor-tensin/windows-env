{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (combine)

import qualified Environment

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

fixNtSymbolPath :: IO ()
fixNtSymbolPath = do
    let env = Environment.CurrentUserEnvironment
    val <- Environment.queryFromRegistry env ntSymbolPath
    let presentPaths = Environment.splitPaths val
    remoteSymbolsPath <- getRemoteSymbolsDirectoryPath
    pdbsPath <- getPdbsDirectoryPath
    let requiredPaths = [pdbsPath, remoteSymbolsPath]
    let missingPaths = filter (`notElem` presentPaths) requiredPaths
    unless (null missingPaths) $ do
        let newval = Environment.joinPaths $ presentPaths ++ missingPaths
        Environment.saveToRegistry env ntSymbolPath newval
  where
    ntSymbolPath = "_NT_SYMBOL_PATH"

main :: IO ()
main = fixNtSymbolPath

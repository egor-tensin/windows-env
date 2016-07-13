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
    oldValue <- Environment.query env varName
    let oldPaths = Environment.pathSplit $ fromMaybe "" oldValue
    pathsToAdd <- addPaths
    let newPaths = union oldPaths pathsToAdd
    when (length oldPaths /= length newPaths) $ do
        let newValue = Environment.pathJoin newPaths
        Environment.engrave env varName newValue
  where
    env = Environment.CurrentUser
    varName = "_NT_SYMBOL_PATH"
    addPaths = sequence [getRemoteSymbolsDirectoryPath, getPdbsDirectoryPath]

main :: IO ()
main = fixNtSymbolPath

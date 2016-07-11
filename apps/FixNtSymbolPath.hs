{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main ( main ) where

import Control.Monad ( unless )
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.FilePath ( combine )

import qualified EnvUtils

getRemoteSymbolsDirectoryPath :: IO String
getRemoteSymbolsDirectoryPath = do
  localPath <- getLocalPath
  createDirectoryIfMissing True localPath
  return $ "SRV*" ++ localPath ++ "*" ++ remotePath
    where
      getLocalPath :: IO String
      getLocalPath = do
        cwd <- getCurrentDirectory
        return $ combine cwd "symbols"
      remotePath :: String
      remotePath = "http://msdl.microsoft.com/download/symbols"

getPdbsDirectoryPath :: IO String
getPdbsDirectoryPath = do
  cwd <- getCurrentDirectory
  let path = combine cwd "pdbs"
  createDirectoryIfMissing True path
  return path

fixNtSymbolPath :: IO ()
fixNtSymbolPath = do
  let env = EnvUtils.CurrentUserEnvironment
  val <- EnvUtils.queryFromRegistry env ntSymbolPath
  let presentPaths = EnvUtils.splitPaths val
  remoteSymbolsPath <- getRemoteSymbolsDirectoryPath
  pdbsPath <- getPdbsDirectoryPath
  let requiredPaths = [pdbsPath, remoteSymbolsPath]
  let missingPaths = filter (`notElem` presentPaths) requiredPaths
  unless (null missingPaths) $ do
    let newval = EnvUtils.joinPaths $ presentPaths ++ missingPaths
    EnvUtils.saveToRegistry env ntSymbolPath newval
  where
    ntSymbolPath = "_NT_SYMBOL_PATH"

main :: IO ()
main = fixNtSymbolPath

{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Environment
    ( RegistryLocation(..)
    , query
    , engrave
    , engraveWithPrompt
    , wipe
    , wipeWithPrompt

    , pathJoin
    , pathSplit
    ) where

import Control.Monad   (when)
import Data.List       (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, isJust)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Graphics.Win32.Window as WinAPI
import qualified System.Win32.Types    as WinAPI

import qualified Registry
import qualified Utils (promptToContinue)

data RegistryLocation
    = CurrentUser
    | AllUsers
    deriving (Eq, Show)

subKeyPath :: RegistryLocation -> Registry.KeyPath
subKeyPath CurrentUser =
    Registry.keyPathFromString "Environment"
subKeyPath AllUsers =
    Registry.keyPathFromString "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"

rootKey :: RegistryLocation -> Registry.RootKey
rootKey CurrentUser = Registry.CurrentUser
rootKey AllUsers    = Registry.LocalMachine

openRootKey :: RegistryLocation -> Registry.KeyHandle
openRootKey = Registry.openRootKey . rootKey

openRegistryKey :: RegistryLocation -> IO Registry.KeyHandle
openRegistryKey env = Registry.openSubKey (openRootKey env) (subKeyPath env)

registryKeyPath :: RegistryLocation -> Registry.KeyPath
registryKeyPath env = Registry.keyPathJoin [Registry.rootKeyPath $ rootKey env, subKeyPath env]

notifyEnvUpdate :: IO ()
notifyEnvUpdate =
    WinAPI.withTString "Environment" $ \lparamPtr -> do
        let wparam = 0
        let lparam = fromIntegral $ WinAPI.castPtrToUINTPtr lparamPtr
        _ <- WinAPI.sendMessage allWindows messageCode wparam lparam
        return ()
  where
    wM_SETTINGCHANGE = 0x1A
    messageCode = wM_SETTINGCHANGE

    allWindows = WinAPI.castUINTPtrToPtr 0xffff

query :: RegistryLocation -> Registry.ValueName -> IO (Maybe Registry.ValueData)
query env name = do
    keyHandle <- openRegistryKey env
    catchIOError (Registry.getString keyHandle name >>= return . Just) emptyIfDoesNotExist
  where
    emptyIfDoesNotExist e = if isDoesNotExistError e then return Nothing else ioError e

engrave :: RegistryLocation -> Registry.ValueName -> Registry.ValueData -> IO ()
engrave env name value = do
    keyHandle <- openRegistryKey env
    Registry.setString keyHandle name value
    notifyEnvUpdate

engraveWithPrompt :: RegistryLocation -> Registry.ValueName -> Registry.ValueData -> IO ()
engraveWithPrompt env name value = do
    putStrLn $ "Saving variable '" ++ name ++ "' to '" ++ registryKeyPath env ++ "'..."
    oldValue <- query env name
    if (isJust oldValue)
        then do
            putStrLn $ "\tOld value: " ++ fromJust oldValue
            putStrLn $ "\tNew value: " ++ value
        else do
            putStrLn $ "\tValue: " ++ value
    agreed <- Utils.promptToContinue
    when agreed $ engrave env name value

wipe :: RegistryLocation -> Registry.ValueName -> IO ()
wipe env name = do
    keyHandle <- openRegistryKey env
    catchIOError (Registry.delValue keyHandle name) ignoreIfDoesNotExist
    notifyEnvUpdate
  where
    ignoreIfDoesNotExist e = if isDoesNotExistError e then return () else ioError e

wipeWithPrompt :: RegistryLocation -> Registry.ValueName -> IO ()
wipeWithPrompt env name = do
    putStrLn $ "Deleting variable '" ++ name ++ "' from '" ++ registryKeyPath env ++ "'..."
    agreed <- Utils.promptToContinue
    when agreed $ wipe env name

pathSep :: String
pathSep = ";"

pathSplit :: String -> [String]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [String] -> String
pathJoin = intercalate pathSep . filter (not . null)

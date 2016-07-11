{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Environment
    ( queryFromRegistry
    , saveToRegistry
    , saveToRegistryWithPrompt
    , wipeFromRegistry
    , wipeFromRegistryWithPrompt
    , getEnv
    , splitPaths
    , joinPaths
    , RegistryBasedEnvironment(..)
    ) where

import           Control.Monad      (liftM, when)
import           Data.List          (intercalate)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import qualified System.Environment (lookupEnv)
import           System.IO.Error    (catchIOError, isDoesNotExistError)

import qualified Graphics.Win32.Window as WinAPI
import qualified System.Win32.Types    as WinAPI

import qualified Registry
import qualified Utils (promptToContinue)

data RegistryBasedEnvironment
    = CurrentUserEnvironment
    | AllUsersEnvironment
    deriving (Eq, Show)

subKeyPath :: RegistryBasedEnvironment -> Registry.KeyPath
subKeyPath CurrentUserEnvironment =
    Registry.keyPathFromString "Environment"
subKeyPath AllUsersEnvironment =
    Registry.keyPathFromString "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"

rootKey :: RegistryBasedEnvironment -> Registry.RootKey
rootKey CurrentUserEnvironment = Registry.CurrentUser
rootKey AllUsersEnvironment    = Registry.LocalMachine

openRootKey :: RegistryBasedEnvironment -> Registry.KeyHandle
openRootKey = Registry.openRootKey . rootKey

openRegistryKey :: RegistryBasedEnvironment -> IO Registry.KeyHandle
openRegistryKey env = Registry.openSubKey (openRootKey env) (subKeyPath env)

registryKeyPath :: RegistryBasedEnvironment -> Registry.KeyPath
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

saveToRegistry :: RegistryBasedEnvironment -> Registry.ValueName -> Registry.ValueData -> IO ()
saveToRegistry env name value = do
    keyHandle <- openRegistryKey env
    Registry.setString keyHandle name value
    notifyEnvUpdate

saveToRegistryWithPrompt :: RegistryBasedEnvironment -> Registry.ValueName -> Registry.ValueData -> IO ()
saveToRegistryWithPrompt env name value = do
    putStrLn $ "Saving variable '" ++ name ++ "' to '" ++ registryKeyPath env ++ "'..."
    oldValue <- queryFromRegistry env name
    putStrLn $ "\tOld value: " ++ oldValue
    putStrLn $ "\tNew value: " ++ value
    agreed <- Utils.promptToContinue
    when agreed $ saveToRegistry env name value

queryFromRegistry :: RegistryBasedEnvironment -> Registry.ValueName -> IO Registry.ValueData
queryFromRegistry env name = do
    keyHandle <- openRegistryKey env
    catchIOError (Registry.getString keyHandle name) emptyIfDoesNotExist
  where
    emptyIfDoesNotExist e = if isDoesNotExistError e then return "" else ioError e

wipeFromRegistry :: RegistryBasedEnvironment -> Registry.ValueName -> IO ()
wipeFromRegistry env name = do
    keyHandle <- openRegistryKey env
    catchIOError (Registry.delValue keyHandle name) ignoreIfDoesNotExist
    notifyEnvUpdate
  where
    ignoreIfDoesNotExist e = if isDoesNotExistError e then return () else ioError e

wipeFromRegistryWithPrompt :: RegistryBasedEnvironment -> Registry.ValueName -> IO ()
wipeFromRegistryWithPrompt env name = do
    putStrLn $ "Deleting variable '" ++ name ++ "' from '" ++ registryKeyPath env ++ "'..."
    agreed <- Utils.promptToContinue
    when agreed $ wipeFromRegistry env name

getEnv :: String -> IO String
getEnv = liftM (fromMaybe "") . System.Environment.lookupEnv

pathSep :: String
pathSep = ";"

splitPaths :: String -> [String]
splitPaths = filter (not . null) . splitOn pathSep

joinPaths :: [String] -> String
joinPaths = intercalate pathSep . filter (not . null)

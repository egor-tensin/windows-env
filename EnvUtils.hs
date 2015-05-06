{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module EnvUtils ( saveToRegistry
                , saveToRegistryWithPrompt
                , queryFromRegistry
                , wipeFromRegistry
                , wipeFromRegistryWithPrompt
                , getEnv
                , splitPaths
                , joinPaths
                , RegistryBasedEnvironment ( CurrentUserEnvironment, AllUsersEnvironment ) ) where

import Control.Monad ( liftM, when )
import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromMaybe )
import qualified System.Environment ( lookupEnv )
import System.IO.Error ( catchIOError, isDoesNotExistError )

import qualified RegUtils
import qualified Utils ( promptToContinue )

data RegistryBasedEnvironment = CurrentUserEnvironment
                              | AllUsersEnvironment
                              deriving (Eq, Show)

registrySubKeyPath :: RegistryBasedEnvironment -> String
registrySubKeyPath CurrentUserEnvironment = "Environment"
registrySubKeyPath AllUsersEnvironment = "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"

registryKey CurrentUserEnvironment = RegUtils.hkcu
registryKey AllUsersEnvironment = RegUtils.hklm

registryKeyPath :: RegistryBasedEnvironment -> String
registryKeyPath CurrentUserEnvironment = "HKCU\\" ++ registrySubKeyPath CurrentUserEnvironment
registryKeyPath AllUsersEnvironment = "HKLM\\" ++ registrySubKeyPath AllUsersEnvironment

saveToRegistry :: RegistryBasedEnvironment -> String -> String -> IO ()
saveToRegistry env = RegUtils.setString (registryKey env) (registrySubKeyPath env)

saveToRegistryWithPrompt :: RegistryBasedEnvironment -> String -> String -> IO ()
saveToRegistryWithPrompt env name value = do
  putStrLn $ "Saving variable '" ++ name ++ "' to '" ++ registryKeyPath env ++ "'..."
  oldValue <- queryFromRegistry env name
  putStrLn $ "\tOld value: " ++ oldValue
  putStrLn $ "\tNew value: " ++ value
  agreed <- Utils.promptToContinue
  when agreed $ saveToRegistry env name value

queryFromRegistry :: RegistryBasedEnvironment -> String -> IO String
queryFromRegistry env name = catchIOError (RegUtils.getString (registryKey env) (registrySubKeyPath env) name) emptyIfDoesNotExist
  where
    emptyIfDoesNotExist :: IOError -> IO String
    emptyIfDoesNotExist e = if isDoesNotExistError e then return "" else ioError e

wipeFromRegistry :: RegistryBasedEnvironment -> String -> IO ()
wipeFromRegistry env name = catchIOError (RegUtils.delValue (registryKey env) (registrySubKeyPath env) name) ignoreIfDoesNotExist
  where
    ignoreIfDoesNotExist :: IOError -> IO ()
    ignoreIfDoesNotExist e = if isDoesNotExistError e then return () else ioError e

wipeFromRegistryWithPrompt :: RegistryBasedEnvironment -> String -> IO ()
wipeFromRegistryWithPrompt env name = do
  putStrLn $ "Deleting variable '" ++ name ++ "' from '" ++ registryKeyPath env ++ "'..."
  agreed <- Utils.promptToContinue
  when agreed $ wipeFromRegistry env name

getEnv :: String -> IO String
getEnv = liftM (fromMaybe "") . System.Environment.lookupEnv

pathSep = ";"

splitPaths :: String -> [String]
splitPaths = filter (not . null) . splitOn pathSep

joinPaths :: [String] -> String
joinPaths = intercalate pathSep . filter (not . null)

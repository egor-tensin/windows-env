-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
--
-- High-level functions for reading and writing Windows environment variables.

module Windows.Environment
    ( Profile(..)
    , profileKeyPath

    , VarName
    , VarValue
    , query
    , engrave
    , wipe

    , pathJoin
    , pathSplit
    ) where

import Data.List       (intercalate)
import Data.List.Split (splitOn)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Windows.Registry as Registry
import           Windows.Utils (notifyEnvironmentUpdate)

data Profile = CurrentUser
             | AllUsers
             deriving (Eq, Show)

profileRootKey :: Profile -> Registry.RootKey
profileRootKey CurrentUser = Registry.CurrentUser
profileRootKey AllUsers    = Registry.LocalMachine

profileRootKeyPath :: Profile -> Registry.KeyPath
profileRootKeyPath = Registry.rootKeyPath . profileRootKey

profileSubKeyPath :: Profile -> Registry.KeyPath
profileSubKeyPath CurrentUser =
    Registry.keyPathFromString "Environment"
profileSubKeyPath AllUsers =
    Registry.keyPathFromString "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"

profileKeyPath :: Profile -> Registry.KeyPath
profileKeyPath profile = Registry.keyPathJoin
    [ profileRootKeyPath profile
    , profileSubKeyPath  profile
    ]

openRootProfileKey :: Profile -> Registry.KeyHandle
openRootProfileKey = Registry.openRootKey . profileRootKey

openProfileKey :: Profile -> IO Registry.KeyHandle
openProfileKey profile = Registry.openSubKey rootKey subKeyPath
  where
    rootKey = openRootProfileKey profile
    subKeyPath = profileSubKeyPath profile

type VarName  = Registry.ValueName
type VarValue = Registry.ValueData

query :: Profile -> VarName -> IO (Maybe VarValue)
query profile name = do
    keyHandle <- openProfileKey profile
    catchIOError (tryQuery keyHandle) ignoreMissing
  where
    tryQuery keyHandle = do
        value <- Registry.getString keyHandle name
        return $ Just value
    ignoreMissing e
        | isDoesNotExistError e = return Nothing
        | otherwise = ioError e

engrave :: Profile -> VarName -> VarValue -> IO ()
engrave profile name value = do
    keyHandle <- openProfileKey profile
    Registry.setString keyHandle name value
    notifyEnvironmentUpdate

wipe :: Profile -> VarName -> IO ()
wipe profile name = do
    keyHandle <- openProfileKey profile
    catchIOError (Registry.delValue keyHandle name) ignoreMissing
    notifyEnvironmentUpdate
  where
    ignoreMissing e
        | isDoesNotExistError e = return ()
        | otherwise = ioError e

pathSep :: VarValue
pathSep = ";"

pathSplit :: VarValue -> [VarValue]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [VarValue] -> VarValue
pathJoin = intercalate pathSep . filter (not . null)
-- |
-- Description : High-level environment variables management functions
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only
--
-- High-level functions for reading and writing Windows environment variables.

{-# LANGUAGE CPP #-}

module WindowsEnv.Environment
    ( Profile(..)
    , profileKeyPath

    , Name
    , Value(..)

    , query
    , engrave
    , wipe

    , pathJoin
    , pathSplit

    , expand
    , expandAll
    ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT(..), catchE, throwE)
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Storable      (sizeOf)
import           System.IO.Error       (isDoesNotExistError, tryIOError)
import qualified System.Win32.Types    as WinAPI

import qualified WindowsEnv.Registry as Registry
import           WindowsEnv.Utils    (notifyEnvironmentUpdate)

data Profile = CurrentUser
             | AllUsers
             deriving (Eq, Show)

profileKeyPath :: Profile -> Registry.KeyPath
profileKeyPath CurrentUser = Registry.KeyPath Registry.CurrentUser ["Environment"]
profileKeyPath AllUsers    = Registry.KeyPath Registry.LocalMachine
    [ "SYSTEM"
    , "CurrentControlSet"
    , "Control"
    , "Session Manager"
    , "Environment"
    ]

type Name = String

data Value = Value
    { valueExpandable :: Bool
    , valueString :: String
    } deriving (Eq, Show)

valueFromRegistry :: Registry.StringValue -> Value
valueFromRegistry (valueType, valueData)
    | valueType == Registry.TypeString = Value False valueData
    | valueType == Registry.TypeExpandableString = Value True valueData
    | otherwise = error "WindowsEnv.Environment: unexpected"

valueToRegistry :: Value -> Registry.StringValue
valueToRegistry value
    | valueExpandable value = (Registry.TypeExpandableString, valueString value)
    | otherwise = (Registry.TypeString, valueString value)

query :: Profile -> Name -> ExceptT IOError IO Value
query profile name = valueFromRegistry <$> Registry.getStringValue (profileKeyPath profile) name

engrave :: Profile -> Name -> Value -> ExceptT IOError IO ()
engrave profile name value = do
    ret <- Registry.setStringValue (profileKeyPath profile) name $ valueToRegistry value
    lift notifyEnvironmentUpdate
    return ret

wipe :: Profile -> Name -> ExceptT IOError IO ()
wipe profile name = do
    ret <- Registry.deleteValue (profileKeyPath profile) name `catchE` ignoreIfMissing
    lift notifyEnvironmentUpdate
    return ret
  where
    ignoreIfMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwE e

pathSep :: String
pathSep = ";"

pathSplit :: String -> [String]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [String] -> String
pathJoin = intercalate pathSep . filter (not . null)

#include "ccall.h"

-- ExpandEnvironmentStrings isn't provided by Win32 (as of version 2.4.0.0).

foreign import WINDOWS_ENV_CCALL unsafe "Windows.h ExpandEnvironmentStringsW"
    c_ExpandEnvironmentStrings :: WinAPI.LPCTSTR -> WinAPI.LPTSTR -> WinAPI.DWORD -> IO WinAPI.ErrCode

expand :: String -> ExceptT IOError IO String
expand value = ExceptT $ tryIOError doExpand
  where
    doExpandIn valuePtr bufferPtr bufferLength = do
        newBufferLength <- WinAPI.failIfZero "ExpandEnvironmentStringsW" $
            c_ExpandEnvironmentStrings valuePtr bufferPtr bufferLength
        let newBufferSize = fromIntegral newBufferLength * sizeOf (undefined :: WinAPI.TCHAR)
        if newBufferLength > bufferLength
            then allocaBytes newBufferSize $ \newBufferPtr -> doExpandIn valuePtr newBufferPtr newBufferLength
            else WinAPI.peekTString bufferPtr
    doExpand = WinAPI.withTString value $ \valuePtr -> doExpandIn valuePtr WinAPI.nullPtr 0

expandAll :: [String] -> ExceptT IOError IO [String]
expandAll = mapM expand

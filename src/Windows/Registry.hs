-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
--
-- Low-level utility functions for reading and writing registry values.

module Windows.Registry
    ( KeyPath
    , keyPathFromString
    , keyPathJoin
    , keyPathSplit

    , KeyHandle
    , openSubKey

    , RootKey(..)
    , rootKeyPath
    , openRootKey

    , ValueName
    , delValue

    , ValueData
    , getString
    , setString
    ) where

import Control.Monad         (unless)
import Data.List             (intercalate)
import Data.List.Split       (splitOn)
import Foreign.ForeignPtr    (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr           (castPtr, plusPtr)
import Foreign.Storable      (peek, poke, sizeOf)
import System.IO.Error
    (catchIOError, doesNotExistErrorType, mkIOError, isDoesNotExistError)

import qualified System.Win32.Registry as WinAPI
import qualified System.Win32.Types    as WinAPI

type KeyName = String
type KeyPath = KeyName

keyPathSep :: KeyPath
keyPathSep = "\\"

keyPathFromString :: String -> KeyPath
keyPathFromString = keyPathJoin . keyPathSplit

keyPathSplit :: KeyPath -> [KeyName]
keyPathSplit = filter (not . null) . splitOn keyPathSep

keyPathJoin :: [KeyName] -> KeyPath
keyPathJoin = intercalate keyPathSep . filter (not . null)

type KeyHandle = WinAPI.HKEY

openSubKey :: KeyHandle -> KeyPath -> IO KeyHandle
openSubKey = WinAPI.regOpenKey

data RootKey = CurrentUser
             | LocalMachine
             deriving (Eq, Show)

rootKeyPath :: RootKey -> KeyName
rootKeyPath CurrentUser  = "HKCU"
rootKeyPath LocalMachine = "HKLM"

openRootKey :: RootKey -> KeyHandle
openRootKey CurrentUser  = WinAPI.hKEY_CURRENT_USER
openRootKey LocalMachine = WinAPI.hKEY_LOCAL_MACHINE

type ValueName = String

raiseDoesNotExistError :: String -> IO a
raiseDoesNotExistError functionName =
    ioError $ mkIOError doesNotExistErrorType functionName Nothing Nothing

raiseUnknownError :: String -> WinAPI.ErrCode -> IO a
raiseUnknownError = WinAPI.failWith

exitCodeSuccess :: WinAPI.ErrCode
exitCodeSuccess = 0

exitCodeFileNotFound :: WinAPI.ErrCode
exitCodeFileNotFound = 0x2

raiseError :: String -> WinAPI.ErrCode -> IO a
raiseError functionName ret
    | ret == exitCodeFileNotFound = raiseDoesNotExistError functionName
    | otherwise                   = raiseUnknownError functionName ret

delValue :: KeyHandle -> ValueName -> IO ()
delValue keyHandle valueName =
    withForeignPtr keyHandle $ \keyPtr ->
    WinAPI.withTString valueName $ \valueNamePtr -> do
        ret <- WinAPI.c_RegDeleteValue keyPtr valueNamePtr
        unless (ret == exitCodeSuccess) $
            raiseError "RegDeleteValue" ret

type ValueType = WinAPI.RegValueType

getType :: KeyHandle -> ValueName -> IO ValueType
getType keyHandle valueName =
    withForeignPtr keyHandle $ \keyPtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \typePtr -> do
        ret <- WinAPI.c_RegQueryValueEx keyPtr valueNamePtr WinAPI.nullPtr typePtr WinAPI.nullPtr WinAPI.nullPtr
        if ret == exitCodeSuccess
            then peek typePtr
            else raiseError "RegQueryValueEx" ret

type ValueData = String

getString :: KeyHandle -> ValueName -> IO ValueData
getString keyHandle valueName =
    withForeignPtr keyHandle $ \keyPtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \dataSizePtr -> do
        poke dataSizePtr 0
        ret <- WinAPI.c_RegQueryValueEx keyPtr valueNamePtr WinAPI.nullPtr WinAPI.nullPtr WinAPI.nullPtr dataSizePtr
        if ret /= exitCodeSuccess
            then raiseError "RegQueryValueEx" ret
            else getStringTerminated keyPtr valueNamePtr dataSizePtr
  where
    getStringTerminated keyPtr valueNamePtr dataSizePtr = do
        dataSize <- peek dataSizePtr
        let newDataSize = dataSize + fromIntegral (sizeOf (undefined :: WinAPI.TCHAR))
        poke dataSizePtr newDataSize
        allocaBytes (fromIntegral newDataSize) $ \dataPtr -> do
            poke (castPtr $ plusPtr dataPtr $ fromIntegral dataSize) '\0'
            ret <- WinAPI.c_RegQueryValueEx keyPtr valueNamePtr WinAPI.nullPtr WinAPI.nullPtr dataPtr dataSizePtr
            if ret == exitCodeSuccess
                then WinAPI.peekTString $ castPtr dataPtr
                else raiseError "RegQueryValueEx" ret

setString :: KeyHandle -> ValueName -> ValueData -> IO ()
setString key name value =
    WinAPI.withTString value $ \valuePtr -> do
        type_ <- catchIOError (getType key name) stringTypeByDefault
        WinAPI.regSetValueEx key name type_ valuePtr valueSize
  where
    stringTypeByDefault e = if isDoesNotExistError e
        then return WinAPI.rEG_SZ
        else ioError e
    valueSize = (length value + 1) * sizeOf (undefined :: WinAPI.TCHAR)

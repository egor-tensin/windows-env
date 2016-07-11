{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Registry
    ( KeyHandle
    , delValue
    , getString
    , setString
    , hkcu
    , hklm
    ) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Graphics.Win32.Window (sendMessage)
import System.IO.Error (mkIOError, doesNotExistErrorType)

import System.Win32.Types
import System.Win32.Registry

newtype KeyHandle = KeyHandle HKEY

getType :: HKEY -> String -> String -> IO (Maybe RegValueType)
getType key subKeyPath valueName =
    bracket (regOpenKey key subKeyPath) regCloseKey $ \hKey ->
    withForeignPtr hKey $ \p_key ->
    withTString valueName $ \p_valueName ->
    alloca $ \p_type -> do
        ret <- c_RegQueryValueEx p_key p_valueName nullPtr p_type nullPtr nullPtr
        case ret of
            0x0 -> do
                type_ <- peek p_type
                return $ Just type_
            0x2 -> return Nothing
            _   -> failWith "RegQueryValueEx" ret

getString :: KeyHandle -> String -> String -> IO String
getString (KeyHandle hKey) subKeyPath valueName =
    bracket (regOpenKey hKey subKeyPath) regCloseKey $ \hSubKey ->
    withForeignPtr hSubKey $ \p_key ->
    withTString valueName $ \p_valueName ->
    alloca $ \dataSizePtr -> do
        poke dataSizePtr 0
        ret <- c_RegQueryValueEx p_key p_valueName nullPtr nullPtr nullPtr dataSizePtr
        case ret of
            0x0 -> do
                dataSize <- peek dataSizePtr
                let newDataSize = dataSize + fromIntegral (sizeOf (undefined :: TCHAR))
                poke dataSizePtr newDataSize
                allocaBytes (fromIntegral newDataSize) $ \dataPtr -> do
                    poke (castPtr $ plusPtr dataPtr $ fromIntegral dataSize) '\0'
                    failUnlessSuccess "RegQueryValueEx" $
                        c_RegQueryValueEx p_key p_valueName nullPtr nullPtr dataPtr dataSizePtr
                    peekTString $ castPtr dataPtr
            0x2 -> ioError $ mkIOError doesNotExistErrorType "RegQueryValueEx" Nothing $ Just (subKeyPath ++ "\\" ++ valueName)
            _   -> failWith "RegQueryValueEx" ret

setString :: KeyHandle -> String -> String -> String -> IO ()
setString (KeyHandle hKey) subKeyPath valueName valueValue =
    bracket (regOpenKey hKey subKeyPath) regCloseKey $ \subKey ->
    withTString valueValue $ \p_valueValue -> do
        type_ <- getType hKey subKeyPath valueName
        regSetValueEx subKey valueName (fromMaybe rEG_SZ type_) p_valueValue $ (length valueValue + 1) * sizeOf (undefined :: TCHAR)
        notifyEnvironmentUpdate

notifyEnvironmentUpdate :: IO ()
notifyEnvironmentUpdate =
    withTString "Environment" $ \p_lparam -> do
        let wparam = 0
        let lparam = fromIntegral $ castPtrToUINTPtr p_lparam
        let hwnd = castUINTPtrToPtr 0xffff
        _ <- sendMessage hwnd wM_SETTINGCHANGE wparam lparam
        return ()
  where
    wM_SETTINGCHANGE = 0x1A

delValue :: KeyHandle -> String -> String -> IO ()
delValue (KeyHandle hKey) subKeyPath valueName =
    bracket (regOpenKey hKey subKeyPath) regCloseKey $ \subKey ->
    withForeignPtr subKey $ \subKeyPtr ->
    withTString valueName $ \p_valueName -> do
        ret <- c_RegDeleteValue subKeyPtr p_valueName
        notifyEnvironmentUpdate
        case ret of
            0x0 -> return ()
            0x2 -> ioError $ mkIOError doesNotExistErrorType "RegQueryValueEx" Nothing $ Just (subKeyPath ++ "\\" ++ valueName)
            _   -> failWith "RegDeleteValue" ret

hkcu :: KeyHandle
hkcu = KeyHandle hKEY_CURRENT_USER

hklm :: KeyHandle
hklm = KeyHandle hKEY_LOCAL_MACHINE

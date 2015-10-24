{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module RegUtils ( delValue
                , getString
                , hkcu
                , hklm
                , setString ) where

import Control.Exception ( bracket )
import Data.Maybe ( fromMaybe )
import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Ptr ( castPtr, plusPtr )
import Foreign.Storable ( peek, poke, sizeOf )
import System.IO.Error ( mkIOError, doesNotExistErrorType )
import System.Win32.Types
import System.Win32.Registry

getType :: HKEY -> String -> String -> IO (Maybe RegValueType)
getType key subKeyPath valueName =
  bracket (regOpenKey key subKeyPath) regCloseKey $ \key ->
  withForeignPtr key $ \keyPtr ->
  withTString valueName $ \valueNamePtr ->
  alloca $ \typePtr -> do
    ret <- c_RegQueryValueEx keyPtr valueNamePtr nullPtr typePtr nullPtr nullPtr
    case ret of
      0x0 -> do
        type' <- peek typePtr
        return $ Just type'
      0x2 -> return Nothing
      _   -> failWith "RegQueryValueEx" ret

getString :: HKEY -> String -> String -> IO String
getString key subKeyPath valueName =
  bracket (regOpenKey key subKeyPath) regCloseKey $ \key ->
  withForeignPtr key $ \keyPtr ->
  withTString valueName $ \valueNamePtr ->
  alloca $ \dataSizePtr -> do
    poke dataSizePtr 0
    ret <- c_RegQueryValueEx keyPtr valueNamePtr nullPtr nullPtr nullPtr dataSizePtr
    case ret of
      0x0 -> do
        dataSize <- peek dataSizePtr
        let newDataSize = dataSize + fromIntegral (sizeOf (undefined :: TCHAR))
        poke dataSizePtr newDataSize
        allocaBytes (fromIntegral newDataSize) $ \dataPtr -> do
          poke (castPtr $ plusPtr dataPtr $ fromIntegral dataSize) '\0'
          failUnlessSuccess "RegQueryValueEx" $
            c_RegQueryValueEx keyPtr valueNamePtr nullPtr nullPtr dataPtr dataSizePtr
          peekTString $ castPtr dataPtr
      0x2 -> ioError $ mkIOError doesNotExistErrorType "RegQueryValueEx" Nothing $ Just (subKeyPath ++ "\\" ++ valueName)
      _   -> failWith "RegQueryValueEx" ret

setString :: HKEY -> String -> String -> String -> IO ()
setString key subKeyPath valueName valueValue =
  bracket (regOpenKey key subKeyPath) regCloseKey $ \subKey ->
  withTString valueValue $ \ptr -> do
    type' <- getType key subKeyPath valueName
    regSetValueEx subKey valueName (fromMaybe rEG_SZ type') ptr $ (length valueValue + 1) * sizeOf (undefined :: TCHAR)
    notifyEnvironmentUpdate

foreign import ccall "BroadcastSystemMessageW"
  c_BroadcastSystemMessage :: DWORD -> LPDWORD -> UINT -> WPARAM -> LPARAM -> IO LONG

notifyEnvironmentUpdate :: IO ()
notifyEnvironmentUpdate =
  withTString "Environment" $ \lparamPtr -> do
    let wparam = fromIntegral $ castPtrToUINTPtr nullPtr
    let lparam = fromIntegral $ castPtrToUINTPtr lparamPtr
    c_BroadcastSystemMessage bSF_POSTMESSAGE nullPtr wM_SETTINGCHANGE wparam lparam
    return ()
      where
        bSF_POSTMESSAGE = 0x10
        wM_SETTINGCHANGE = 0x1A

delValue :: HKEY -> String -> String -> IO ()
delValue key subKeyPath valueName =
  bracket (regOpenKey key subKeyPath) regCloseKey $ \subKey ->
  withForeignPtr subKey $ \subKeyPtr ->
  withTString valueName $ \valueNamePtr -> do
    ret <- c_RegDeleteValue subKeyPtr valueNamePtr
    notifyEnvironmentUpdate
    case ret of
      0x0 -> return ()
      0x2 -> ioError $ mkIOError doesNotExistErrorType "RegQueryValueEx" Nothing $ Just (subKeyPath ++ "\\" ++ valueName)
      _   -> failWith "RegDeleteValue" ret

hkcu = hKEY_CURRENT_USER
hklm = hKEY_LOCAL_MACHINE

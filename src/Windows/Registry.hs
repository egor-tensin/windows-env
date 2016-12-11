-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
--
-- Low-level utility functions for reading and writing registry values.

module Windows.Registry
    ( IsKeyPath(..)
    , RootKey(..)
    , KeyPath(..)

    , ValueName
    , ValueType
    , ValueData

    , open
    , close

    , deleteValue

    , queryValue

    , getValue
    , getExpandedString

    , setValue
    , setString
    , setExpandableString
    ) where

import           Data.Bits             ((.|.))
import qualified Data.ByteString       as B
import           Data.List             (intercalate)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf16LE, encodeUtf16LE)
import           Control.Exception     (bracket)
import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Storable      (peek, poke)
import           System.IO.Error       (catchIOError)

import qualified System.Win32.Types    as WinAPI
import qualified System.Win32.Registry as WinAPI

type Handle = WinAPI.HKEY

class IsKeyPath a where
    openUnsafe :: a -> IO Handle

close :: Handle -> IO ()
close = WinAPI.regCloseKey

open :: IsKeyPath a => a -> IO (Either IOError Handle)
open a = catchIOError (Right <$> openUnsafe a) $ return . Left

data RootKey = CurrentUser
             | LocalMachine
             deriving (Eq)

instance IsKeyPath RootKey where
    openUnsafe CurrentUser = return WinAPI.hKEY_CURRENT_USER
    openUnsafe LocalMachine = return WinAPI.hKEY_LOCAL_MACHINE

instance Show RootKey where
    show CurrentUser = "HKCU"
    show LocalMachine = "HKLM"

data KeyPath = KeyPath RootKey [String]

pathSep :: String
pathSep = "\\"

instance IsKeyPath KeyPath where
    openUnsafe (KeyPath root path) = do
        rootHandle <- openUnsafe root
        WinAPI.regOpenKey rootHandle $ intercalate pathSep path

instance Show KeyPath where
    show (KeyPath root path) = intercalate pathSep $ show root : path

type ValueName = String
type ValueType = WinAPI.DWORD
type ValueData = (ValueType, B.ByteString)

encodeString :: String -> B.ByteString
encodeString = encodeUtf16LE . T.pack

decodeString :: ValueData -> String
decodeString (_, valueData) = T.unpack . decodeUtf16LE $ valueData

openCloseCatch :: IsKeyPath a => a -> (Handle -> IO b) -> IO (Either IOError b)
openCloseCatch keyPath f = catchIOError (fmap Right openClose) $ return . Left
  where
    openClose = bracket (openUnsafe keyPath) close f

foreign import ccall unsafe "Windows.h RegQueryValueExW"
    c_RegQueryValueEx :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.LPDWORD -> WinAPI.LPDWORD -> WinAPI.LPBYTE -> WinAPI.LPDWORD -> IO WinAPI.ErrCode

foreign import ccall unsafe "Windows.h RegSetValueExW"
    c_RegSetValueEx :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.DWORD -> WinAPI.DWORD -> WinAPI.LPBYTE -> WinAPI.DWORD -> IO WinAPI.ErrCode

foreign import ccall unsafe "Windows.h RegGetValueW"
    c_RegGetValue :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.LPCTSTR -> WinAPI.DWORD -> WinAPI.LPDWORD -> WinAPI.LPBYTE -> WinAPI.LPDWORD -> IO WinAPI.ErrCode

queryValue :: IsKeyPath a => a -> ValueName -> IO (Either IOError ValueData)
queryValue keyPath valueName =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \dataSizePtr -> do
        poke dataSizePtr 0
        WinAPI.failUnlessSuccess "RegQueryValueExW" $ c_RegQueryValueEx keyHandlePtr valueNamePtr WinAPI.nullPtr WinAPI.nullPtr WinAPI.nullPtr dataSizePtr
        dataSize <- fromIntegral <$> peek dataSizePtr
        alloca $ \dataTypePtr ->
            allocaBytes dataSize $ \bufferPtr -> do
                WinAPI.failUnlessSuccess "RegQueryValueExW" $ c_RegQueryValueEx keyHandlePtr valueNamePtr WinAPI.nullPtr dataTypePtr bufferPtr dataSizePtr
                buffer <- peekArray dataSize bufferPtr
                dataType <- peek dataTypePtr
                return (dataType, B.pack buffer)

getValue :: IsKeyPath a => a -> ValueName -> [ValueType] -> IO (Either IOError ValueData)
getValue keyPath valueName allowedTypes =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \dataSizePtr -> do
        poke dataSizePtr 0
        let flags = foldr (.|.) 0 allowedTypes
        WinAPI.failUnlessSuccess "RegGetValueW" $ c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr flags WinAPI.nullPtr WinAPI.nullPtr dataSizePtr
        dataSize <- fromIntegral <$> peek dataSizePtr
        alloca $ \dataTypePtr ->
            allocaBytes dataSize $ \bufferPtr -> do
                WinAPI.failUnlessSuccess "RegGetValueW" $ c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr flags dataTypePtr bufferPtr dataSizePtr
                buffer <- peekArray dataSize bufferPtr
                dataType <- peek dataTypePtr
                return (dataType, B.pack buffer)

getExpandedString :: IsKeyPath a => a -> ValueName -> IO (Either IOError String)
getExpandedString keyPath valueName = do
    valueData <- getValue keyPath valueName [WinAPI.rEG_SZ, WinAPI.rEG_EXPAND_SZ]
    return $ fmap decodeString valueData

setValue :: IsKeyPath a => a -> ValueName -> ValueData -> IO (Either IOError ())
setValue keyPath valueName (valueType, valueData) =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr -> do
        let buffer = B.unpack valueData
        let dataSize = B.length valueData
        allocaBytes dataSize $ \bufferPtr -> do
            pokeArray bufferPtr buffer
            WinAPI.failUnlessSuccess "RegSetValueExW" $ c_RegSetValueEx keyHandlePtr valueNamePtr 0 valueType bufferPtr (fromIntegral dataSize)

setString :: IsKeyPath a => a -> ValueName -> String -> IO (Either IOError ())
setString keyPath valueName valueData =
    setValue keyPath valueName (WinAPI.rEG_SZ, encodeString valueData)

setExpandableString :: IsKeyPath a => a -> ValueName -> String -> IO (Either IOError ())
setExpandableString keyPath valueName valueData =
    setValue keyPath valueName (WinAPI.rEG_EXPAND_SZ, encodeString valueData)

deleteValue :: IsKeyPath a => a -> ValueName -> IO (Either IOError ())
deleteValue keyPath valueName =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    WinAPI.failUnlessSuccess "RegDeleteValueW" $ WinAPI.c_RegDeleteValue keyHandlePtr valueNamePtr

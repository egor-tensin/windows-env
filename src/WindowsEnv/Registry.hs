-- |
-- Description : Lower-level registry access wrappers
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only
--
-- Lower-level functions for reading and writing registry values.

{-# LANGUAGE CPP #-}

module WindowsEnv.Registry
    ( IsKeyPath(..)
    , RootKey(..)
    , KeyPath(..)

    , ValueName
    , ValueType(..)
    , Value
    , StringValue

    , openKey
    , closeKey

    , deleteValue

    , queryValue
    , queryValueType

    , getValue
    , GetValueFlag(..)
    , getValueType
    , getStringValue

    , setValue
    , setStringValue
    ) where

import           Control.Exception     (bracket)
import           Control.Monad.Trans.Except (ExceptT(..))
import           Data.Bits             ((.|.))
import qualified Data.ByteString       as B
import           Data.List             (intercalate)
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf16LE, encodeUtf16LE)
import           Data.Tuple            (swap)
import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Storable      (peek, poke)
import           System.IO.Error       (tryIOError)
import qualified System.Win32.Types    as WinAPI
import qualified System.Win32.Registry as WinAPI

type Handle = WinAPI.HKEY

class IsKeyPath a where
    openKeyUnsafe :: a -> IO Handle

closeKey :: Handle -> IO ()
closeKey = WinAPI.regCloseKey

openKey :: IsKeyPath a => a -> IO (Either IOError Handle)
openKey keyPath = tryIOError $ openKeyUnsafe keyPath

withHandle :: IsKeyPath a => a -> (Handle -> IO b) -> ExceptT IOError IO b
withHandle keyPath f = ExceptT $ tryIOError doWithHandle
  where
    doWithHandle = bracket (openKeyUnsafe keyPath) closeKey f

data RootKey = CurrentUser
             | LocalMachine
             deriving (Eq)

instance IsKeyPath RootKey where
    openKeyUnsafe CurrentUser = return WinAPI.hKEY_CURRENT_USER
    openKeyUnsafe LocalMachine = return WinAPI.hKEY_LOCAL_MACHINE

instance Show RootKey where
    show CurrentUser = "HKCU"
    show LocalMachine = "HKLM"

data KeyPath = KeyPath RootKey [String]

keyPathSep :: String
keyPathSep = "\\"

instance IsKeyPath KeyPath where
    openKeyUnsafe (KeyPath root path) = do
        rootHandle <- openKeyUnsafe root
        WinAPI.regOpenKey rootHandle $ intercalate keyPathSep path

instance Show KeyPath where
    show (KeyPath root path) = intercalate keyPathSep $ show root : path

type ValueName = String

data ValueType = TypeNone
               | TypeBinary
               | TypeDWord
               | TypeDWordBE
               | TypeQWord
               | TypeString
               | TypeMultiString
               | TypeExpandableString
               | TypeLink
               deriving (Eq, Show)

instance Enum ValueType where
    fromEnum = fromJust . flip lookup valueTypeNumbers
    toEnum = fromJust . flip lookup (map swap valueTypeNumbers)

valueTypeNumbers :: [(ValueType, Int)]
valueTypeNumbers =
    [ (TypeNone,     0)
    , (TypeBinary,   3)
    , (TypeDWord,    4)
    , (TypeDWordBE,  5)
    , (TypeQWord,   11)
    , (TypeString,   1)
    , (TypeMultiString,      7)
    , (TypeExpandableString, 2)
    , (TypeLink,     6)
    ]

type Value = (ValueType, B.ByteString)
type StringValue = (ValueType, String)

encodeString :: StringValue -> Value
encodeString (valueType, valueData) = (valueType, encodeUtf16LE addLastZero)
  where
    addLastZero
        | T.null text = text
        | T.last text == '\0' = text
        | otherwise = T.snoc text '\0'
    text = T.pack valueData

decodeString :: Value -> StringValue
decodeString (valueType, valueData) = (valueType, T.unpack dropLastZero)
  where
    dropLastZero
        | T.null text = text
        | otherwise = T.takeWhile (/= '\0') text
    text = decodeUtf16LE valueData

#include "windows_cconv.h"

-- These aren't provided by Win32 (as of version 2.4.0.0).

foreign import WINDOWS_CCONV unsafe "Windows.h RegQueryValueExW"
    c_RegQueryValueEx :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.LPDWORD -> WinAPI.LPDWORD -> WinAPI.LPBYTE -> WinAPI.LPDWORD -> IO WinAPI.ErrCode

foreign import WINDOWS_CCONV unsafe "Windows.h RegSetValueExW"
    c_RegSetValueEx :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.DWORD -> WinAPI.DWORD -> WinAPI.LPBYTE -> WinAPI.DWORD -> IO WinAPI.ErrCode

foreign import WINDOWS_CCONV unsafe "Windows.h RegGetValueW"
    c_RegGetValue :: WinAPI.PKEY -> WinAPI.LPCTSTR -> WinAPI.LPCTSTR -> WinAPI.DWORD -> WinAPI.LPDWORD -> WinAPI.LPBYTE -> WinAPI.LPDWORD -> IO WinAPI.ErrCode

queryValue :: IsKeyPath a => a -> ValueName -> ExceptT IOError IO Value
queryValue keyPath valueName =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \valueSizePtr -> do
        poke valueSizePtr 0
        WinAPI.failUnlessSuccess "RegQueryValueExW" $
            c_RegQueryValueEx keyHandlePtr valueNamePtr WinAPI.nullPtr WinAPI.nullPtr WinAPI.nullPtr valueSizePtr
        valueSize <- fromIntegral <$> peek valueSizePtr
        alloca $ \valueTypePtr ->
            allocaBytes valueSize $ \bufferPtr -> do
                WinAPI.failUnlessSuccess "RegQueryValueExW" $
                    c_RegQueryValueEx keyHandlePtr valueNamePtr WinAPI.nullPtr valueTypePtr bufferPtr valueSizePtr
                buffer <- B.pack <$> peekArray valueSize bufferPtr
                valueType <- toEnum . fromIntegral <$> peek valueTypePtr
                return (valueType, buffer)

queryValueType :: IsKeyPath a => a -> ValueName -> ExceptT IOError IO ValueType
queryValueType keyPath valueName =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \valueTypePtr -> do
        WinAPI.failUnlessSuccess "RegQueryValueExW" $
            c_RegQueryValueEx keyHandlePtr valueNamePtr WinAPI.nullPtr valueTypePtr WinAPI.nullPtr WinAPI.nullPtr
        toEnum . fromIntegral <$> peek valueTypePtr

data GetValueFlag = RestrictAny
                  | RestrictNone
                  | RestrictBinary
                  | RestrictDWord
                  | RestrictQWord
                  | RestrictString
                  | RestrictMultiString
                  | RestrictExpandableString
                  | DoNotExpand
                  deriving (Eq, Show)

instance Enum GetValueFlag where
    fromEnum = fromJust . flip lookup getValueFlagNumbers
    toEnum = fromJust . flip lookup (map swap getValueFlagNumbers)

getValueFlagNumbers :: [(GetValueFlag, Int)]
getValueFlagNumbers =
    [ (RestrictAny,    0x0000ffff)
    , (RestrictNone,   0x00000001)
    , (RestrictBinary, 0x00000008)
    , (RestrictDWord,  0x00000010)
    , (RestrictQWord,  0x00000040)
    , (RestrictString, 0x00000002)
    , (RestrictMultiString,      0x00000020)
    , (RestrictExpandableString, 0x00000004)
    , (DoNotExpand,    0x10000000)
    ]

collapseGetValueFlags :: Num a => [GetValueFlag] -> a
collapseGetValueFlags = fromIntegral . foldr ((.|.) . fromEnum) 0

getValue :: IsKeyPath a => a -> ValueName -> [GetValueFlag] -> ExceptT IOError IO Value
getValue keyPath valueName flags =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \valueSizePtr -> do
        poke valueSizePtr 0
        WinAPI.failUnlessSuccess "RegGetValueW" $
            c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr collapsedFlags WinAPI.nullPtr WinAPI.nullPtr valueSizePtr
        bufferCapacity <- fromIntegral <$> peek valueSizePtr
        alloca $ \valueTypePtr ->
            allocaBytes bufferCapacity $ \bufferPtr -> do
                WinAPI.failUnlessSuccess "RegGetValueW" $
                    c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr collapsedFlags valueTypePtr bufferPtr valueSizePtr
                bufferSize <- fromIntegral <$> peek valueSizePtr
                buffer <- B.pack <$> peekArray bufferSize bufferPtr
                valueType <- toEnum . fromIntegral <$> peek valueTypePtr
                return (valueType, buffer)
  where
    collapsedFlags = collapseGetValueFlags $ DoNotExpand : flags

getValueType :: IsKeyPath a => a -> ValueName -> [GetValueFlag] -> ExceptT IOError IO ValueType
getValueType keyPath valueName flags =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \valueTypePtr -> do
        WinAPI.failUnlessSuccess "RegGetValueW" $
            c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr collapsedFlags valueTypePtr WinAPI.nullPtr WinAPI.nullPtr
        toEnum . fromIntegral <$> peek valueTypePtr
  where
    collapsedFlags = collapseGetValueFlags $ DoNotExpand : flags

getStringValue :: IsKeyPath a => a -> ValueName -> ExceptT IOError IO StringValue
getStringValue keyPath valueName =
    decodeString <$> getValue keyPath valueName [RestrictExpandableString, RestrictString]

setValue :: IsKeyPath a => a -> ValueName -> Value -> ExceptT IOError IO ()
setValue keyPath valueName (valueType, valueData) =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    allocaBytes bufferSize $ \bufferPtr -> do
        pokeArray bufferPtr buffer
        WinAPI.failUnlessSuccess "RegSetValueExW" $
            c_RegSetValueEx keyHandlePtr valueNamePtr 0 rawValueType bufferPtr (fromIntegral bufferSize)
  where
    rawValueType = fromIntegral $ fromEnum valueType
    buffer = B.unpack valueData
    bufferSize = B.length valueData

setStringValue :: IsKeyPath a => a -> ValueName -> StringValue -> ExceptT IOError IO ()
setStringValue keyPath valueName value =
    setValue keyPath valueName $ encodeString value

deleteValue :: IsKeyPath a => a -> ValueName -> ExceptT IOError IO ()
deleteValue keyPath valueName =
    withHandle keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    WinAPI.failUnlessSuccess "RegDeleteValueW" $
        WinAPI.c_RegDeleteValue keyHandlePtr valueNamePtr

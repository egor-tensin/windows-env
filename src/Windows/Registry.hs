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
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)
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
    fromEnum = fromJust . flip lookup valueTypeTable
    toEnum = fromJust . flip lookup (map swap valueTypeTable)

valueTypeTable :: [(ValueType, Int)]
valueTypeTable =
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

type ValueData = (ValueType, B.ByteString)

encodeString :: String -> B.ByteString
encodeString str = encodeUtf16LE addLastZero
  where
    text = T.pack str
    addLastZero | T.null text = text
                | T.last text == '\0' = text
                | otherwise = T.snoc text '\0'

decodeString :: ValueData -> String
decodeString (_, bytes) = T.unpack dropLastZero
  where
    text = decodeUtf16LE bytes
    dropLastZero | T.null text = text
                 | T.last text == '\0' = T.init text
                 | otherwise = text

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
                return (toEnum $ fromIntegral dataType, B.pack buffer)

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
    fromEnum = fromJust . flip lookup getValueFlagsTable
    toEnum = fromJust . flip lookup (map swap getValueFlagsTable)

getValueFlagsTable :: [(GetValueFlag, Int)]
getValueFlagsTable =
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

getValue :: IsKeyPath a => a -> ValueName -> [GetValueFlag] -> IO (Either IOError ValueData)
getValue keyPath valueName flagList =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    alloca $ \dataSizePtr -> do
        poke dataSizePtr 0
        let flags = fromIntegral $ foldr (.|.) 0 $ map fromEnum flagList
        WinAPI.failUnlessSuccess "RegGetValueW" $ c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr flags WinAPI.nullPtr WinAPI.nullPtr dataSizePtr
        bufferCapacity <- fromIntegral <$> peek dataSizePtr
        alloca $ \dataTypePtr ->
            allocaBytes bufferCapacity $ \bufferPtr -> do
                WinAPI.failUnlessSuccess "RegGetValueW" $ c_RegGetValue keyHandlePtr WinAPI.nullPtr valueNamePtr flags dataTypePtr bufferPtr dataSizePtr
                bufferSize <- fromIntegral <$> peek dataSizePtr
                buffer <- peekArray bufferSize bufferPtr
                dataType <- peek dataTypePtr
                return (toEnum $ fromIntegral dataType, B.pack buffer)

getExpandedString :: IsKeyPath a => a -> ValueName -> IO (Either IOError String)
getExpandedString keyPath valueName = do
    valueData <- getValue keyPath valueName [RestrictString, RestrictExpandableString]
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
            WinAPI.failUnlessSuccess "RegSetValueExW" $ c_RegSetValueEx keyHandlePtr valueNamePtr 0 (fromIntegral $ fromEnum valueType) bufferPtr (fromIntegral dataSize)

setString :: IsKeyPath a => a -> ValueName -> String -> IO (Either IOError ())
setString keyPath valueName valueData =
    setValue keyPath valueName (TypeString, encodeString valueData)

setExpandableString :: IsKeyPath a => a -> ValueName -> String -> IO (Either IOError ())
setExpandableString keyPath valueName valueData =
    setValue keyPath valueName (TypeExpandableString, encodeString valueData)

deleteValue :: IsKeyPath a => a -> ValueName -> IO (Either IOError ())
deleteValue keyPath valueName =
    openCloseCatch keyPath $ \keyHandle ->
    withForeignPtr keyHandle $ \keyHandlePtr ->
    WinAPI.withTString valueName $ \valueNamePtr ->
    WinAPI.failUnlessSuccess "RegDeleteValueW" $ WinAPI.c_RegDeleteValue keyHandlePtr valueNamePtr

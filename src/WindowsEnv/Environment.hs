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

    , VarName
    , VarValue
    , expand
    , query
    , engrave
    , engraveForce
    , wipe

    , pathJoin
    , pathSplit
    ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT(..))
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Storable      (sizeOf)
import           System.IO.Error       (catchIOError)
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

type VarName  = String
type VarValue = String

#include "ccall.h"

-- ExpandEnvironmentStrings isn't provided by Win32 (as of version 2.4.0.0).

foreign import WINDOWS_ENV_CCALL unsafe "Windows.h ExpandEnvironmentStringsW"
    c_ExpandEnvironmentStrings :: WinAPI.LPCTSTR -> WinAPI.LPTSTR -> WinAPI.DWORD -> IO WinAPI.ErrCode

expand :: VarValue -> ExceptT IOError IO VarValue
expand value = ExceptT $ catchIOError (Right <$> doExpand) (return . Left)
  where
    doExpandIn valuePtr bufferPtr bufferLength = do
        newBufferLength <- WinAPI.failIfZero "ExpandEnvironmentStringsW" $
            c_ExpandEnvironmentStrings valuePtr bufferPtr bufferLength
        let newBufferSize = (fromIntegral newBufferLength) * sizeOf (undefined :: WinAPI.TCHAR)
        if newBufferLength > bufferLength
            then allocaBytes newBufferSize $ \newBufferPtr -> doExpandIn valuePtr newBufferPtr newBufferLength
            else WinAPI.peekTString bufferPtr
    doExpand = WinAPI.withTString value $ \valuePtr -> doExpandIn valuePtr WinAPI.nullPtr 0

query :: Profile -> VarName -> ExceptT IOError IO VarValue
query profile name = Registry.getString (profileKeyPath profile) name

engrave :: Profile -> VarName -> VarValue -> ExceptT IOError IO ()
engrave profile name value = do
    ret <- Registry.setStringPreserveType (profileKeyPath profile) name value
    lift notifyEnvironmentUpdate
    return ret

engraveForce :: Profile -> VarName -> VarValue -> ExceptT IOError IO ()
engraveForce profile name value = do
    ret <- Registry.setString (profileKeyPath profile) name value
    lift notifyEnvironmentUpdate
    return ret

wipe :: Profile -> VarName -> ExceptT IOError IO ()
wipe profile name = do
    ret <- Registry.deleteValue (profileKeyPath profile) name
    lift notifyEnvironmentUpdate
    return ret

pathSep :: VarValue
pathSep = ";"

pathSplit :: VarValue -> [VarValue]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [VarValue] -> VarValue
pathJoin = intercalate pathSep . filter (not . null)

-- |
-- Copyright   : (c) 2016 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Windows.Utils
    ( notifyEnvironmentUpdate
    ) where

import           Foreign.C.Types          (CIntPtr(..))
import qualified Graphics.Win32.GDI.Types as WinAPI
import qualified Graphics.Win32.Message   as WinAPI
import qualified System.Win32.Types       as WinAPI

foreign import ccall "Windows.h SendNotifyMessageW"
    c_SendNotifyMessage :: WinAPI.HWND -> WinAPI.WindowMessage -> WinAPI.WPARAM -> WinAPI.LPARAM -> IO WinAPI.LRESULT

notifyEnvironmentUpdate :: IO ()
notifyEnvironmentUpdate =
    WinAPI.withTString "Environment" $ \lparamPtr -> do
        let wparam = 0
        let lparam = fromIntegral $ WinAPI.castPtrToUINTPtr lparamPtr
        _ <- c_SendNotifyMessage allWindows messageCode wparam lparam
        return ()
  where
    messageCode = WinAPI.wM_WININICHANGE
    hWND_BROADCAST = WinAPI.castUINTPtrToPtr 0xffff
    allWindows = hWND_BROADCAST

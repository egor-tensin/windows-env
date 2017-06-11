-- |
-- Copyright   : (c) 2016 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Utils.PromptMessage
    ( oldNewMessage
    , newMessage
    , wipeMessage
    ) where

import Text.Printf (printf)

import qualified WindowsEnv

oldNewMessage :: WindowsEnv.Profile -> WindowsEnv.Name -> WindowsEnv.Value -> WindowsEnv.Value -> String
oldNewMessage profile name oldValue newValue =
    descrMsg ++ oldValueMsg ++ newValueMsg
  where
    profileKey = WindowsEnv.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name $ show profileKey
    oldValueMsg = printf "\tOld value: %s\n" $ WindowsEnv.valueString oldValue
    newValueMsg = printf "\tNew value: %s\n" $ WindowsEnv.valueString newValue

newMessage :: WindowsEnv.Profile -> WindowsEnv.Name -> WindowsEnv.Value -> String
newMessage profile name newValue =
    descrMsg ++ newValueMsg
  where
    profileKey = WindowsEnv.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name $ show profileKey
    newValueMsg = printf "\tNew value: %s\n" $ WindowsEnv.valueString newValue

wipeMessage :: WindowsEnv.Profile -> WindowsEnv.Name -> String
wipeMessage profile name =
    printf "Deleting variable '%s' from '%s'...\n" name $ show profileKey
  where
    profileKey = WindowsEnv.profileKeyPath profile

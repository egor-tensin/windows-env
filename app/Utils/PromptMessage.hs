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

oldNewMessage :: WindowsEnv.Profile -> WindowsEnv.VarName -> String -> String -> String
oldNewMessage profile name oldValue newValue =
    descrMsg ++ oldValueMsg ++ newValueMsg
  where
    profileKey = WindowsEnv.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name (show profileKey)
    oldValueMsg = printf "\tOld value: %s\n" oldValue
    newValueMsg = printf "\tNew value: %s\n" newValue

newMessage :: WindowsEnv.Profile -> WindowsEnv.VarName -> String -> String
newMessage profile name newValue =
    descrMsg ++ newValueMsg
  where
    profileKey = WindowsEnv.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name (show profileKey)
    newValueMsg = printf "\tNew value: %s\n" newValue

wipeMessage :: WindowsEnv.Profile -> WindowsEnv.VarName -> String
wipeMessage profile name =
    printf "Deleting variable '%s' from '%s'...\n" name (show profileKey)
  where
    profileKey = WindowsEnv.profileKeyPath profile

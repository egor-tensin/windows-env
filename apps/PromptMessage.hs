-- |
-- Copyright   : (c) 2016 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module PromptMessage
    ( oldNewMessage
    , newMessage
    , wipeMessage
    ) where

import Text.Printf (printf)

import qualified Windows.Environment as Env

oldNewMessage :: Env.Profile -> Env.VarName -> Env.VarValue -> Env.VarValue -> String
oldNewMessage profile name oldValue newValue =
    descrMsg ++ oldValueMsg ++ newValueMsg
  where
    profileKey = Env.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name (show profileKey)
    oldValueMsg = printf "\tOld value: %s\n" oldValue
    newValueMsg = printf "\tNew value: %s\n" newValue

newMessage :: Env.Profile -> Env.VarName -> Env.VarValue -> String
newMessage profile name newValue =
    descrMsg ++ newValueMsg
  where
    profileKey = Env.profileKeyPath profile
    descrMsg = printf "Saving variable '%s' to '%s'...\n" name (show profileKey)
    newValueMsg = printf "\tNew value: %s\n" newValue

wipeMessage :: Env.Profile -> Env.VarName -> String
wipeMessage profile name =
    printf "Deleting variable '%s' from '%s'...\n" name (show profileKey)
  where
    profileKey = Env.profileKeyPath profile

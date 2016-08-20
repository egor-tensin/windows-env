-- |
-- Copyright   : (c) 2016 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module PromptMessage
    ( engraveMessage
    , wipeMessage
    ) where

import Data.Maybe  (isJust)
import Text.Printf (printf)

import qualified Windows.Environment as Env

engraveMessage :: Env.Profile -> Env.VarName -> Maybe Env.VarValue -> Env.VarValue -> String
engraveMessage profile name oldValue newValue =
    descriptionMsg ++ oldValueMsg ++ newValueMsg
  where
    profileKey = Env.profileKeyPath profile

    descriptionMsg = printf "Saving variable '%s' to '%s'...\n" name profileKey

    oldValueMsg = maybe "" (printf "\tOld value: %s\n") oldValue
    newValueMsg
        | isJust oldValue = printf "\tNew value: %s\n" newValue
        | otherwise       = printf "\tValue: %s\n"     newValue

wipeMessage :: Env.Profile -> Env.VarName -> String
wipeMessage profile name =
    printf "Deleting variable '%s' from '%s'...\n" name profileKey
  where
    profileKey = Env.profileKeyPath profile

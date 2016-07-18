{-
 - Copyright 2016 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module PromptMessage
    ( engraveMessage
    , wipeMessage
    ) where

import Data.Maybe  (fromJust, isJust)
import Text.Printf (printf)

import qualified Windows.Environment as Env

engraveMessage :: Env.Profile -> Env.VarName -> Maybe Env.VarValue -> Env.VarValue -> String
engraveMessage profile name oldValue newValue =
    warning ++ values
  where
    warning = printf "Saving variable '%s' to '%s'...\n" name $ Env.profileKeyPath profile

    values
        | isJust oldValue = oldValueMsg ++ newValueMsg
        | otherwise       = valueMsg

    oldValueMsg = printf "\tOld value: %s\n" $ fromJust oldValue
    newValueMsg = printf "\tNew value: %s\n" newValue
    valueMsg = printf "\tValue: %s\n" newValue

wipeMessage :: Env.Profile -> Env.VarName -> String
wipeMessage profile name =
    printf "Deleting variable '%s' from '%s'...\n" name $ Env.profileKeyPath profile

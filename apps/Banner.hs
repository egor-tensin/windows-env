{-
 - Copyright 2016 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Banner
    ( engraveBanner
    , wipeBanner
    ) where

import Data.Maybe  (fromJust, isJust)
import Text.Printf (printf)

import qualified Windows.Environment as Env

engraveBanner :: Env.Profile -> Env.VarName -> Maybe Env.VarValue -> Env.VarValue -> String
engraveBanner profile name oldValue newValue =
    warning ++ valuesStr
  where
    warning = printf "Saving variable '%s' to '%s'...\n" name (Env.profileKeyPath profile)
    valuesStr
        | isJust oldValue = oldValueStr ++ newValueStr
        | otherwise       = theValueStr
    oldValueStr = printf "\tOld value: %s\n" $ fromJust oldValue
    newValueStr = printf "\tNew value: %s\n" newValue
    theValueStr = printf "\tValue: %s\n" newValue

wipeBanner :: Env.Profile -> Env.VarName -> String
wipeBanner profile name =
    printf "Deleting variable '%s' from '%s'...\n" name (Env.profileKeyPath profile)

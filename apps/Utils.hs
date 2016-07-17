{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Utils
    ( withPrompt
    , withoutPrompt

    , engraveBanner
    , wipeBanner
    ) where

import Control.Monad (liftM, void, when)
import Data.Maybe    (fromJust, isJust)
import Data.Char     (toLower)
import System.IO     (hFlush, stdout)
import Text.Printf   (printf)

import Environment   (Profile, profileKeyPath, VarName, VarValue)

prompt :: String -> IO String
prompt banner = do
    putStr banner
    hFlush stdout
    getLine

promptYesNo :: String -> IO Bool
promptYesNo banner = do
    response <- liftM (map toLower) $ prompt banner
    if response `elem` yeses
        then return True
        else if response `elem` noes
            then return False
            else promptToContinue
  where
    yeses = ["y", "yes"]
    noes = ["n", "no"]
    
promptToContinue :: IO Bool
promptToContinue = promptYesNo "Continue? (y/n) "

withPrompt :: String -> IO a -> IO Bool
withPrompt banner m = do
    putStr banner
    hFlush stdout
    agreed <- promptToContinue
    when agreed $ void m
    return agreed

withoutPrompt :: IO a -> IO Bool
withoutPrompt m = m >> return True

engraveBanner :: Profile -> VarName -> Maybe VarValue -> VarValue -> String
engraveBanner profile name oldValue newValue =
    header ++ values
  where
    header = printf "Saving variable '%s' to '%s'...\n" name (profileKeyPath profile)
    values = if isJust oldValue
        then printf "\tOld value: %s\n\tNew value: %s\n" (fromJust oldValue) newValue
        else printf "\tValue: %s\n" newValue

wipeBanner :: Profile -> VarName -> String
wipeBanner profile name =
    printf "Deleting variable '%s' from '%s'...\n" name (profileKeyPath profile)

-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Prompt
    ( withPrompt
    , withoutPrompt
    ) where

import Control.Monad (void, when)
import Data.Char     (toLower)
import System.IO     (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine

promptYesNo :: String -> IO Bool
promptYesNo msg = do
    response <- map toLower <$> prompt msg
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
withPrompt msg m = do
    putStr msg
    hFlush stdout
    agreed <- promptToContinue
    when agreed $ void m
    return agreed

withoutPrompt :: IO a -> IO Bool
withoutPrompt m = m >> return True

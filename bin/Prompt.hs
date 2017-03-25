-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : portable

module Prompt
    ( withPrompt
    , withoutPrompt
    ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
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

withPrompt :: String -> ExceptT IOError IO a -> ExceptT IOError IO Bool
withPrompt msg m = do
    lift $ do
        putStr msg
        hFlush stdout
    agreed <- lift promptToContinue
    when agreed $ void m
    return agreed

withoutPrompt :: ExceptT IOError IO a -> ExceptT IOError IO Bool
withoutPrompt m = m >> return True

{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Options.Applicative

import qualified Environment

data Options = Options
    { global :: Bool
    , name :: String
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> globalOption
    <*> nameArg
  where
    globalOption = switch $
        long "global" <> short 'g' <>
        help "Whether to unset for all users"
    nameArg = argument str $
        metavar "NAME" <>
        help "Variable name"

main :: IO ()
main = execParser parser >>= unsetEnv
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Unset environment variables"

unsetEnv :: Options -> IO ()
unsetEnv options = Environment.wipeFromRegistryWithPrompt env $ name options
  where
    env | global options = Environment.AllUsersEnvironment
        | otherwise      = Environment.CurrentUserEnvironment

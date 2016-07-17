{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (void)

import Options.Applicative

import qualified Environment

import qualified Utils

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: Environment.VarName
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> optYes
    <*> optGlobalDesc
    <*> optNameDesc
  where
    optYes = switch $
        long "yes" <> short 'y' <>
        help "Skip confirmation prompt"
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Unset for all users"
    optNameDesc = argument str $
        metavar "NAME" <>
        help "Variable name"

main :: IO ()
main = execParser parser >>= unsetEnv
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Unset environment variable"

unsetEnv :: Options -> IO ()
unsetEnv options = void $ prompt confirmationBanner $ Environment.wipe profile varName
  where
    confirmationBanner = Utils.wipeBanner profile varName

    varName = optName options

    forAllUsers = optGlobal options
    profile = if forAllUsers
        then Environment.AllUsers
        else Environment.CurrentUser

    skipPrompt = optYes options
    prompt = if skipPrompt
        then const Utils.withoutPrompt
        else Utils.withPrompt

{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (void)

import           Options.Applicative
import qualified Windows.Environment as Env

import qualified Utils

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: Env.VarName
    , optValue  :: Env.VarValue
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> optYesDesc
    <*> optGlobalDesc
    <*> optNameDesc
    <*> optValueDesc
  where
    optYesDesc = switch $
        long "yes" <> short 'y' <>
        help "Skip confirmation prompt"
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Set for all users"
    optNameDesc = argument str $
        metavar "NAME" <>
        help "Variable name"
    optValueDesc = argument str $
        metavar "VALUE" <>
        help "Variable value"

main :: IO ()
main = execParser parser >>= setEnv
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Set environment variable"

setEnv :: Options -> IO ()
setEnv options = void $ prompt confirmationBanner $ Env.engrave profile varName varValue
  where
    confirmationBanner = Utils.engraveBanner profile varName Nothing varValue

    varName = optName options
    varValue = optValue options

    forAllUsers = optGlobal options
    profile = if forAllUsers
        then Env.AllUsers
        else Env.CurrentUser

    skipPrompt = optYes options
    prompt = if skipPrompt
        then const Utils.withoutPrompt
        else Utils.withPrompt

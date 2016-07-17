{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Options.Applicative hiding (value)

import qualified Environment

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: String
    , optValue  :: String
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
setEnv options = engrave varValue
  where
    varName = optName options
    varValue = optValue options

    forAllUsers = optGlobal options
    env | forAllUsers = Environment.AllUsers
        | otherwise   = Environment.CurrentUser

    skipPrompt = optYes options
    engrave value = if skipPrompt
        then Environment.engrave       env varName value
        else Environment.engravePrompt env varName value >> return ()

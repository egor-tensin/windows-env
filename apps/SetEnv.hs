{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Options.Applicative hiding (value)

import qualified Environment

data Options = Options
    { optGlobal :: Bool
    , optName   :: String
    , optValue  :: String
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> optGlobalDesc
    <*> optNameDesc
    <*> optValueDesc
  where
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Whether to set for all users"
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
        fullDesc <> progDesc "Set environment variables"

setEnv :: Options -> IO ()
setEnv options = Environment.engraveWithPrompt env varName varValue
  where
    env | optGlobal options = Environment.AllUsers
        | otherwise         = Environment.CurrentUser
    varName = optName options
    varValue = optValue options

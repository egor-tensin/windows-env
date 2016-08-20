-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad (void)

import           Options.Applicative
import qualified Windows.Environment as Env

import Prompt
import PromptMessage

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: Env.VarName
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optYesDesc
    <*> optGlobalDesc
    <*> optNameDesc
  where
    optYesDesc = switch
         $ long "yes" <> short 'y'
        <> help "Skip confirmation prompt"
    optGlobalDesc = switch
         $ long "global" <> short 'g'
        <> help "Unset for all users"
    optNameDesc = argument str
         $ metavar "NAME"
        <> help "Variable name"

main :: IO ()
main = execParser parser >>= unsetEnv
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Unset environment variables"

unsetEnv :: Options -> IO ()
unsetEnv options = void $ promptAnd wipe
  where
    varName = optName options

    forAllUsers = optGlobal options
    profile
        | forAllUsers = Env.AllUsers
        | otherwise   = Env.CurrentUser

    skipPrompt = optYes options
    promptAnd
        | skipPrompt = withoutPrompt
        | otherwise  = withPrompt $ wipeMessage profile varName

    wipe = Env.wipe profile varName

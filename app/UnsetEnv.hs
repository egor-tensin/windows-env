-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Main (main) where

import Control.Monad   (void)
import Control.Monad.Trans.Except (runExceptT)
import System.IO.Error (ioError)

import Options.Applicative

import qualified WindowsEnv

import Utils.Prompt
import Utils.PromptMessage

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: WindowsEnv.Name
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
        fullDesc <> progDesc "Delete environment variables"

unsetEnv :: Options -> IO ()
unsetEnv options = runExceptT doUnsetEnv >>= either ioError return
  where
    varName = optName options
    forAllUsers = optGlobal options
    skipPrompt = optYes options

    profile
        | forAllUsers = WindowsEnv.AllUsers
        | otherwise   = WindowsEnv.CurrentUser

    doUnsetEnv = void $ promptAnd $ WindowsEnv.wipe profile varName

    promptAnd
        | skipPrompt = withoutPrompt
        | otherwise  = withPrompt $ wipeMessage profile varName

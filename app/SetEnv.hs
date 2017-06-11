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
    , optValue  :: String
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optYesDesc
    <*> optGlobalDesc
    <*> optNameDesc
    <*> optValueDesc
  where
    optYesDesc = switch
         $ long "yes" <> short 'y'
        <> help "Skip confirmation prompt"
    optGlobalDesc = switch
         $ long "global" <> short 'g'
        <> help "Set for all users"
    optNameDesc = argument str
         $ metavar "NAME"
        <> help "Variable name"
    optValueDesc = argument str
         $ metavar "VALUE"
        <> help "Variable value"

main :: IO ()
main = execParser parser >>= setEnv
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Define environment variables"

setEnv :: Options -> IO ()
setEnv options = runExceptT doSetEnv >>= either ioError return
  where
    varName = optName options
    varValue = optValue options

    forAllUsers = optGlobal options
    profile
        | forAllUsers = WindowsEnv.AllUsers
        | otherwise   = WindowsEnv.CurrentUser

    skipPrompt = optYes options

    doSetEnv = do
        expanded <- WindowsEnv.expand varValue
        let expandable = expanded == varValue
        let newValue = WindowsEnv.Value expandable varValue
        promptAndEngrave newValue

    promptAndEngrave newValue = do
        let promptAnd = if skipPrompt
            then withoutPrompt
            else withPrompt $ newMessage profile varName newValue
        let engrave = WindowsEnv.engrave profile varName newValue
        void $ promptAnd engrave

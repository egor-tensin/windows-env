-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad   (void)
import Control.Monad.Trans.Except (runExceptT)
import System.IO.Error (ioError)

import Options.Applicative

import qualified Windows.Environment as Env

import Prompt
import PromptMessage

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    , optName   :: Env.VarName
    , optValue  :: Env.VarValue
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
        fullDesc <> progDesc "Set environment variables"

setEnv :: Options -> IO ()
setEnv options = runExceptT doSetEnv >>= either ioError return
  where
    varName = optName options
    varValue = optValue options

    forAllUsers = optGlobal options
    profile
        | forAllUsers = Env.AllUsers
        | otherwise   = Env.CurrentUser

    skipPrompt = optYes options
    promptAnd
        | skipPrompt = withoutPrompt
        | otherwise  = withPrompt $ newMessage profile varName varValue

    engrave = Env.engraveForce profile varName varValue

    doSetEnv = void $ promptAnd engrave

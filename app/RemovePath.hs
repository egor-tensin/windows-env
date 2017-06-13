-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Main (main) where

import Control.Monad   (void, when)
import Control.Monad.Trans.Except (catchE, runExceptT, throwE)
import System.IO.Error (ioError, isDoesNotExistError)

import Options.Applicative

import qualified WindowsEnv

import Utils.Prompt
import Utils.PromptMessage

data Options = Options
    { optName   :: WindowsEnv.Name
    , optYes    :: Bool
    , optGlobal :: Bool
    , optPaths  :: [String]
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optNameDesc
    <*> optYesDesc
    <*> optGlobalDesc
    <*> optPathsDesc
  where
    optNameDesc = strOption
         $ long "name" <> short 'n'
        <> metavar "NAME" <> value "PATH"
        <> help "Variable name ('PATH' by default)"
    optYesDesc = switch
         $ long "yes" <> short 'y'
        <> help "Skip confirmation prompt"
    optGlobalDesc = switch
         $ long "global" <> short 'g'
        <> help "Remove for all users"
    optPathsDesc = many $ argument str
         $ metavar "PATH"
        <> help "Directories to remove"

main :: IO ()
main = execParser parser >>= removePath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Remove directories from your PATH"

removePath :: Options -> IO ()
removePath options = runExceptT doRemovePath >>= either ioError return
  where
    varName = optName options
    pathsToRemove = optPaths options
    forAllUsers = optGlobal options
    skipPrompt = optYes options

    ignoreMissing e
        | isDoesNotExistError e = return ()
        | otherwise = throwE e

    doRemovePath = do
        removePathFrom WindowsEnv.CurrentUser `catchE` ignoreMissing
        when forAllUsers $
            removePathFrom WindowsEnv.AllUsers `catchE` ignoreMissing

    removePathFrom profile = do
        oldValue <- WindowsEnv.query profile varName
        let expandable = WindowsEnv.valueExpandable oldValue
        let joined = WindowsEnv.valueString oldValue
        let split = WindowsEnv.pathSplit joined
        let remaining = filter (`notElem` pathsToRemove) split
        when (length split /= length remaining) $ do
            let newValue = WindowsEnv.Value expandable (WindowsEnv.pathJoin remaining)
            promptAndEngrave profile oldValue newValue

    promptAndEngrave profile oldValue newValue = do
        let promptAnd = if skipPrompt
            then withoutPrompt
            else withPrompt $ oldNewMessage profile varName oldValue newValue
        let engrave = WindowsEnv.engrave profile varName newValue
        void $ promptAnd engrave

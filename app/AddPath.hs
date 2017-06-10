-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Main (main) where

import Control.Monad   (unless, void)
import Control.Monad.Trans.Except (catchE, runExceptT, throwE)
import Data.List       ((\\), nub)
import System.IO.Error (ioError, isDoesNotExistError)

import Options.Applicative

import qualified WindowsEnv

import Utils.Prompt
import Utils.PromptMessage

data Options = Options
    { optName   :: WindowsEnv.VarName
    , optYes     :: Bool
    , optGlobal  :: Bool
    , optPrepend :: Bool
    , optPaths   :: [WindowsEnv.VarValue]
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optNameDesc
    <*> optYesDesc
    <*> optGlobalDesc
    <*> optPrependDesc
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
        <> help "Add for all users"
    optPrependDesc = switch
         $ long "prepend" <> short 'p'
        <> help "Prepend to the variable (instead of appending)"
    optPathsDesc = many $ argument str
         $ metavar "PATH"
        <> help "Directories to add"

main :: IO ()
main = execParser parser >>= addPath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Add directories to your PATH"

addPath :: Options -> IO ()
addPath options = runExceptT doAddPath >>= either ioError return
  where
    varName = optName options
    pathsToAdd = nub $ optPaths options

    forAllUsers = optGlobal options
    profile
        | forAllUsers = WindowsEnv.AllUsers
        | otherwise   = WindowsEnv.CurrentUser

    skipPrompt = optYes options

    prepend = optPrepend options
    append xs ys
        | prepend = ys ++ xs
        | otherwise = xs ++ ys

    emptyIfMissing e
        | isDoesNotExistError e = return ""
        | otherwise = throwE e

    doAddPath = do
        oldValue <- WindowsEnv.query profile varName `catchE` emptyIfMissing
        let oldPaths = WindowsEnv.pathSplit oldValue
        let newPaths = pathsToAdd \\ oldPaths
        unless (null newPaths) $ do
            let newValue = WindowsEnv.pathJoin $ append oldPaths newPaths
            let promptAnd = if skipPrompt
                then withoutPrompt
                else withPrompt $ oldNewMessage profile varName oldValue newValue
            let engrave = WindowsEnv.engrave profile varName newValue
            void $ promptAnd engrave

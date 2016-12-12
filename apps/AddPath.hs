-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad   (void, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.List       (union)
import System.IO.Error (ioError, isDoesNotExistError)

import           Options.Applicative
import qualified Windows.Environment as Env

import Prompt
import PromptMessage

data Options = Options
    { optName   :: Env.VarName
    , optYes    :: Bool
    , optGlobal :: Bool
    , optPaths  :: [Env.VarValue]
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
        <> help "Add for all users"
    optPathsDesc = many $ argument str
         $ metavar "PATH"
        <> help "Directories to add"

main :: IO ()
main = execParser parser >>= addPath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Add directories to your PATH"

addPath :: Options -> IO ()
addPath options = do
    ret <- runExceptT $ doAddPath
    either ioError return ret
  where
    varName = optName options
    pathsToAdd = optPaths options

    forAllUsers = optGlobal options
    profile
        | forAllUsers = Env.AllUsers
        | otherwise   = Env.CurrentUser

    skipPrompt = optYes options

    emptyIfMissing e | isDoesNotExistError e = return ""
                     | otherwise = throwE e

    doAddPath = do
        oldValue <- Env.query profile varName `catchE` emptyIfMissing
        let oldPaths = Env.pathSplit oldValue
        let newPaths = oldPaths `union` pathsToAdd
        when (length oldPaths /= length newPaths) $ do
            let newValue = Env.pathJoin newPaths
            let promptAnd = if skipPrompt
                then withoutPrompt
                else withPrompt $ engraveMessage profile varName oldValue newValue
            let engrave = Env.engrave profile varName newValue
            lift $ void $ promptAnd $ runExceptT engrave

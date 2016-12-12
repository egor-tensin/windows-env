-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad   (void, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.List       ((\\))
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
removePath options = do
    ret <- runExceptT $ doRemovePath
    either ioError return ret
  where
    varName = optName options
    pathsToRemove = optPaths options

    forAllUsers = optGlobal options

    skipPrompt = optYes options

    ignoreMissing e
        | isDoesNotExistError e = return ""
        | otherwise = throwE e

    doRemovePath = do
        removePathFrom Env.CurrentUser
        when forAllUsers $
            removePathFrom Env.AllUsers

    removePathFrom profile = do
        oldValue <- Env.query profile varName `catchE` ignoreMissing
        doRemovePathFrom profile oldValue

    doRemovePathFrom profile oldValue = do
        let oldPaths = Env.pathSplit oldValue
        let newPaths = oldPaths \\ pathsToRemove
        when (length oldPaths /= length newPaths) $ do
            let newValue = Env.pathJoin newPaths
            let promptAnd = if skipPrompt
                then withoutPrompt
                else withPrompt $ engraveMessage profile varName oldValue newValue
            let engrave = Env.engrave profile varName newValue
            lift $ void $ promptAnd $ runExceptT engrave

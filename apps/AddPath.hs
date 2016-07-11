{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (mapM_, when)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, stderr)

import qualified Environment

main :: IO ()
main = do
    rawArgs <- getArgs
    case getOpt Permute optionDescription rawArgs of
        (actions, args, []) -> do
            options <- foldl (>>=) (return defaultOptions) actions
            addPath args options
        (_, _, errorMessages) -> exitWithUsageErrors errorMessages

addPath :: [String] -> Options -> IO ()
addPath paths options = do
    missingPaths <- dropIncludedPaths paths
    when (not $ null missingPaths) $ do
        oldPath <- Environment.queryFromRegistry (env options) (name options)
        Environment.saveToRegistryWithPrompt (env options) (name options) $ Environment.joinPaths $ missingPaths ++ [oldPath]
  where
    dropIncludedPaths paths = do
        currentPath <- Environment.getEnv $ name options
        return $ filter (flip notElem $ Environment.splitPaths currentPath) paths

data Options = Options
    { name :: String
    , env :: Environment.RegistryBasedEnvironment
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { name = "PATH"
    , env = Environment.CurrentUserEnvironment
    }

buildHelpMessage :: IO String
buildHelpMessage = do
    header <- buildHeader
    return $ usageInfo header optionDescription
  where
    buildHeader :: IO String
    buildHeader = do
        progName <- getProgName
        return $ "Usage: " ++ progName ++ " [OPTIONS...] [PATH...]\nOptions:"

exitWithHelpMessage :: a -> IO b
exitWithHelpMessage _ = do
    helpMessage <- buildHelpMessage
    putStr helpMessage
    exitSuccess

exitWithUsageErrors :: [String] -> IO a
exitWithUsageErrors errorMessages = do
    hPutStr stderr $ concatMap ("Usage error: " ++) errorMessages
    helpMessage <- buildHelpMessage
    hPutStr stderr helpMessage
    exitFailure

invalidNumberOfArguments :: IO a
invalidNumberOfArguments = exitWithUsageErrors ["invalid number of arguments\n"]

optionDescription :: [OptDescr (Options -> IO Options)]
optionDescription =
    [ Option "n" ["name"] (ReqArg (\s opts -> return opts { name = s }) "NAME") "set the variable name ('PATH' by default)"
    , Option "g" ["global"] (NoArg $ \opts -> return opts { env = Environment.AllUsersEnvironment }) "add the path for all users"
    , Option "h" ["help"] (NoArg exitWithHelpMessage) "show this message and exit"
    ]

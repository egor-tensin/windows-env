{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

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
            case args of
                [name] -> unsetEnv name options
                _ -> invalidNumberOfArguments
        (_, _, errorMessages) -> exitWithUsageErrors errorMessages

unsetEnv :: String -> Options -> IO ()
unsetEnv name options = Environment.wipeFromRegistryWithPrompt (env options) name

data Options = Options
    { env :: Environment.RegistryBasedEnvironment
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { env = Environment.CurrentUserEnvironment
    }

buildHelpMessage :: IO String
buildHelpMessage = do
    header <- buildHeader
    return $ usageInfo header optionDescription
  where
    buildHeader = do
        progName <- getProgName
        return $ "Usage: " ++ progName ++ " [OPTIONS...] NAME\nOptions:"

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
    [ Option "g" ["global"] (NoArg $ \opts -> return opts { env = Environment.AllUsersEnvironment }) "delete from the registry key for all users"
    , Option "h" ["help"] (NoArg exitWithHelpMessage) "show this message and exit"
    ]
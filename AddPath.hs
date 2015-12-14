{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main ( main ) where

import Control.Monad ( when )
import System.Console.GetOpt
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStr, stderr )

import qualified EnvUtils

main :: IO ()
main = do
  rawArgs <- getArgs
  case getOpt Permute optionDescription rawArgs of
    (actions, args, []) -> do
      options <- foldl (>>=) (return defaultOptions) actions
      case args of
        [path] -> addPath path options
        _ -> invalidNumberOfArguments
    (_, _, errorMessages) -> exitWithUsageErrors errorMessages

addPath :: String -> Options -> IO ()
addPath path options = do
  oldVal <- EnvUtils.getEnv $ name options
  when (notElem path $ EnvUtils.splitPaths oldVal) $ do
    oldValFromReg <- EnvUtils.queryFromRegistry (env options) (name options)
    EnvUtils.saveToRegistryWithPrompt (env options) (name options) $ EnvUtils.joinPaths [path,oldValFromReg]

data Options = Options { name :: String
                       , env :: EnvUtils.RegistryBasedEnvironment }
                       deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options { name = "PATH"
                         , env = EnvUtils.CurrentUserEnvironment }

buildHelpMessage :: IO String
buildHelpMessage = do
  header <- buildHeader
  return $ usageInfo header optionDescription
    where
      buildHeader :: IO String
      buildHeader = do
        progName <- getProgName
        return $ "Usage: " ++ progName ++ " [OPTIONS...] PATH\nOptions:"

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
optionDescription = [
    Option "n" ["name"] (ReqArg (\s opts -> return opts { name = s }) "NAME") "set the variable name ('PATH' by default)",
    Option "g" ["global"] (NoArg $ \opts -> return opts { env = EnvUtils.AllUsersEnvironment }) "add the path for all users",
    Option "h" ["help"] (NoArg exitWithHelpMessage) "show this message and exit"
  ]

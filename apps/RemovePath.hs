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
      removePath args options
    (_, _, errorMessages) -> exitWithUsageErrors errorMessages

removePath :: [String] -> Options -> IO ()
removePath paths options = do
  let varName = name options
  userVal <- EnvUtils.queryFromRegistry EnvUtils.CurrentUserEnvironment varName
  let userValParts = EnvUtils.splitPaths userVal
  let newUserValParts = filter (`notElem` paths) userValParts
  when (length userValParts /= length newUserValParts) $ do
    EnvUtils.saveToRegistryWithPrompt EnvUtils.CurrentUserEnvironment varName $ EnvUtils.joinPaths newUserValParts
  when (global options) $ do
    globalVal <- EnvUtils.queryFromRegistry EnvUtils.AllUsersEnvironment varName
    let globalValParts = EnvUtils.splitPaths globalVal
    let newGlobalValParts = filter (`notElem` paths) globalValParts
    when (length globalValParts /= length newGlobalValParts) $ do
      EnvUtils.saveToRegistryWithPrompt EnvUtils.AllUsersEnvironment varName $ EnvUtils.joinPaths newGlobalValParts

data Options = Options { name :: String
                       , global :: Bool }
                       deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options { name = "PATH"
                         , global = False }

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
optionDescription = [
    Option "n" ["name"] (ReqArg (\s opts -> return opts { name = s }) "NAME") "set the variable name ('PATH' by default)",
    Option "g" ["global"] (NoArg $ \opts -> return opts { global = True }) "remove the path for all users",
    Option "h" ["help"] (NoArg exitWithHelpMessage) "show this message and exit"
  ]

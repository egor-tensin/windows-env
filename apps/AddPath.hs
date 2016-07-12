{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (when)

import Options.Applicative

import qualified Environment

data Options = Options
    { name :: String
    , global :: Bool
    , paths :: [String]
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> nameOption
    <*> globalOption
    <*> pathArgs
  where
    nameOption = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Specify variable name ('PATH' by default)"
    globalOption = switch $
        long "global" <> short 'g' <>
        help "Whether to add for all users"
    pathArgs = many $ argument str $
        metavar "PATH" <>
        help "Directory path(s)"

main :: IO ()
main = execParser parser >>= addPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Add directories to your PATH"

addPath :: Options -> IO ()
addPath options = do
    missingPaths <- dropIncludedPaths $ paths options
    when (not $ null missingPaths) $ do
        oldPath <- Environment.queryFromRegistry env $ name options
        Environment.saveToRegistryWithPrompt env (name options) $ Environment.joinPaths $ missingPaths ++ [oldPath]
  where
    dropIncludedPaths paths = do
        currentPath <- Environment.getEnv $ name options
        return $ filter (flip notElem $ Environment.splitPaths currentPath) paths
    env | global options = Environment.AllUsersEnvironment
        | otherwise      = Environment.CurrentUserEnvironment

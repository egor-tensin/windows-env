{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import System.Directory (doesDirectoryExist)

import Options.Applicative

import qualified Environment

data Options = Options
    { name :: String
    } deriving (Eq, Show)

options :: Parser Options
options = Options <$> nameOption
  where
    nameOption = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Specify variable name ('PATH' by default)"

main :: IO ()
main = execParser parser >>= listPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "List directories in your PATH"

listPath :: Options -> IO ()
listPath options = do
    val <- Environment.getEnv $ name options
    mapM_ printPath $ Environment.splitPaths val
  where
    printPath p = do
        exists <- doesDirectoryExist p
        putStrLn $ (if exists then "+" else "-") ++ " " ++ p

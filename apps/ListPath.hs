{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad      (liftM)
import Data.Maybe         (fromMaybe)
import System.Directory   (doesDirectoryExist)
import System.Environment (lookupEnv)

import Options.Applicative

import qualified Environment

data Options = Options
    { optName :: String
    } deriving (Eq, Show)

options :: Parser Options
options = Options <$> optNameDesc
  where
    optNameDesc = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Variable name ('PATH' by default)"

main :: IO ()
main = execParser parser >>= listPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "List directories in your PATH"

listPath :: Options -> IO ()
listPath options = do
    oldValue <- getEnv varName
    let oldPaths = Environment.pathSplit oldValue
    mapM_ printPath oldPaths
  where
    varName = optName options
    getEnv = liftM (fromMaybe "") . lookupEnv

    printPath p = do
        exists <- doesDirectoryExist p
        putStrLn $ (if exists then "+" else "-") ++ " " ++ p

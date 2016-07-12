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

getEnv :: String -> IO String
getEnv = liftM (fromMaybe "") . lookupEnv

listPath :: Options -> IO ()
listPath options = do
    val <- getEnv $ name options
    mapM_ printPath $ Environment.pathSplit val
  where
    printPath p = do
        exists <- doesDirectoryExist p
        putStrLn $ (if exists then "+" else "-") ++ " " ++ p

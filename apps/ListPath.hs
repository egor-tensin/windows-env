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

import           Options.Applicative
import qualified Windows.Environment as Env

data Options = Options
    { optName :: Env.VarName
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options <$> optNameDesc
  where
    optNameDesc = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Variable name ('PATH' by default)"

main :: IO ()
main = execParser parser >>= listPath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "List directories in your PATH"

listPath :: Options -> IO ()
listPath options = do
    oldValue <- query
    printPaths $ Env.pathSplit oldValue
  where
    varName = optName options

    query = liftM (fromMaybe "") $ lookupEnv varName

    prefix exists
        | exists    = "+ "
        | otherwise = "- "

    formatPath exists path = prefix exists ++ path

    printPath path = do
        exists <- doesDirectoryExist path
        putStrLn $ formatPath exists path

    printPaths = mapM_ printPath

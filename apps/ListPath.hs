{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad      (filterM)
import Data.Maybe         (fromMaybe)
import System.Directory   (doesDirectoryExist)
import System.Environment (lookupEnv)

import           Options.Applicative
import qualified Windows.Environment as Env

data WhichPaths = All | ExistingOnly | MissingOnly
                deriving (Eq, Show)

shouldListPath :: WhichPaths -> Env.VarValue -> IO Bool
shouldListPath All = return . const True
shouldListPath ExistingOnly = doesDirectoryExist
shouldListPath MissingOnly  = fmap not . doesDirectoryExist

data Options = Options
    { optName :: Env.VarName
    , optWhichPaths :: WhichPaths
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optNameDesc
    <*> optWhichPathsDesc
  where
    optNameDesc = strOption
         $ long "name" <> short 'n'
        <> metavar "NAME" <> value "PATH"
        <> help "Variable name ('PATH' by default)"
    optWhichPathsDesc = pure All
        <|> flag' ExistingOnly (long "existing" <> short 'e'
            <> help "List existing paths only")
        <|> flag' MissingOnly (long "missing" <> short 'm'
            <> help "List missing paths only")

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
    whichPaths = optWhichPaths options

    query = fromMaybe "" <$> lookupEnv varName

    printPaths paths =
        filterM (shouldListPath whichPaths) paths >>= mapM_ putStrLn

-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Main (main) where

import Control.Monad      (filterM)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Maybe         (fromMaybe)
import System.Directory   (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.IO.Error    (ioError)

import Options.Applicative

import qualified WindowsEnv

data WhichPaths = All | ExistingOnly | MissingOnly
                deriving (Eq, Show)

shouldListPath :: WhichPaths -> WindowsEnv.VarValue -> IO Bool
shouldListPath All = return . const True
shouldListPath ExistingOnly = doesDirectoryExist
shouldListPath MissingOnly  = fmap not . doesDirectoryExist

data Source = Environment | Registry WindowsEnv.Profile
            deriving (Eq, Show)

data Options = Options
    { optName       :: WindowsEnv.VarName
    , optWhichPaths :: WhichPaths
    , optSource     :: Source
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optNameDesc
    <*> optWhichPathsDesc
    <*> optSourceDesc
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
    optSourceDesc = pure Environment
        <|> flag' (Registry WindowsEnv.CurrentUser) (long "user" <> short 'u'
            <> help "List current user's paths only")
        <|> flag' (Registry WindowsEnv.AllUsers) (long "global" <> short 'g'
            <> help "List global (all users') paths only")

main :: IO ()
main = execParser parser >>= listPaths
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "List directories in your PATH"

data ExpandedPath = ExpandedPath
    { pathOriginal :: WindowsEnv.VarValue
    , pathExpanded :: WindowsEnv.VarValue
    } deriving (Eq, Show)

splitAndExpand :: WindowsEnv.VarValue -> ExceptT IOError IO [ExpandedPath]
splitAndExpand pathValue = do
    expandedOnce <- expandOnce
    zipWith ExpandedPath originalPaths <$>
        if length expandedOnce == length originalPaths
            then return expandedOnce
            else expandEach
  where
    originalPaths = WindowsEnv.pathSplit pathValue
    expandOnce = WindowsEnv.pathSplit <$> WindowsEnv.expand pathValue
    expandEach = mapM WindowsEnv.expand originalPaths

listPaths :: Options -> IO ()
listPaths options = runExceptT doListPaths >>= either ioError return
  where
    varName = optName options
    whichPaths = optWhichPaths options

    query = queryFrom $ optSource options

    queryFrom Environment = lift $ fromMaybe "" <$> lookupEnv varName
    queryFrom (Registry profile) = WindowsEnv.query profile varName

    filterPaths = filterM (shouldListPath whichPaths . pathExpanded)

    doListPaths = do
        paths <- query >>= splitAndExpand
        lift $ do
            pathsToPrint <- filterPaths paths
            mapM_ (putStrLn . pathOriginal) pathsToPrint

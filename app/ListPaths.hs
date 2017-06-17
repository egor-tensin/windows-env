-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Main (main) where

import Control.Monad      (filterM)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.Version       (showVersion)
import System.Directory   (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.IO.Error    (ioError)

import Options.Applicative

import qualified Paths_windows_env as Meta
import qualified WindowsEnv

import Utils.Path

data WhichPaths = All | ExistingOnly | MissingOnly
                deriving (Eq, Show)

shouldListPath :: WhichPaths -> ExpandedPath -> IO Bool
shouldListPath All = return . const True
shouldListPath ExistingOnly = doesDirectoryExist . pathExpanded
shouldListPath MissingOnly  = fmap not . doesDirectoryExist . pathExpanded

data Source = Environment | Registry WindowsEnv.Profile
            deriving (Eq, Show)

data Options = Options
    { optName       :: WindowsEnv.Name
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
    parser = info (helper <*> versioner <*> optionParser)
         $ fullDesc
        <> progDesc "List directories in your PATH"
    versioner = infoOption (showVersion Meta.version)
         $ long "version"
        <> help "Show version"
        <> hidden

listPaths :: Options -> IO ()
listPaths options = runExceptT doListPaths >>= either ioError return
  where
    varName = optName options
    whichPaths = optWhichPaths options

    query = queryFrom $ optSource options

    queryFrom Environment = lift $ WindowsEnv.Value False
        <$> fromMaybe ""
        <$> lookupEnv varName
    queryFrom (Registry profile) = WindowsEnv.query profile varName

    doListPaths = do
        varValue <- query
        split <- pathExpandValue varValue
        lift $ do
            wanted <- filterM (shouldListPath whichPaths) split
            mapM_ (putStrLn . pathOriginal) wanted

-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad      (filterM)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe         (fromMaybe)
import System.Directory   (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.IO.Error    (ioError)

import Options.Applicative

import qualified Windows.Environment as Env

data WhichPaths = All | ExistingOnly | MissingOnly
                deriving (Eq, Show)

shouldListPath :: WhichPaths -> Env.VarValue -> IO Bool
shouldListPath All = return . const True
shouldListPath ExistingOnly = doesDirectoryExist
shouldListPath MissingOnly  = fmap not . doesDirectoryExist

data Source = Environment | Registry Env.Profile
            deriving (Eq, Show)

data Options = Options
    { optName       :: Env.VarName
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
        <|> flag' (Registry Env.CurrentUser) (long "user" <> short 'u'
            <> help "List current user's paths only")
        <|> flag' (Registry Env.AllUsers) (long "global" <> short 'g'
            <> help "List global (all users') paths only")

main :: IO ()
main = execParser parser >>= listPath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "List directories in your PATH"

listPath :: Options -> IO ()
listPath options = runExceptT doListPath >>= either ioError return
  where
    varName = optName options
    whichPaths = optWhichPaths options
    source = optSource options

    query = queryFrom source

    queryFrom Environment = lift $ fromMaybe "" <$> lookupEnv varName
    queryFrom (Registry profile) = Env.query profile varName

    doListPath = do
        paths <- query
        lift $ printPaths $ Env.pathSplit paths

    printPaths paths =
        filterM (shouldListPath whichPaths) paths >>= mapM_ putStrLn

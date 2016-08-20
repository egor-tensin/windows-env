-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental

module Main (main) where

import Control.Monad    (when)
import Data.List        (union)
import Data.Maybe       (fromMaybe)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath  (combine)

import           Options.Applicative
import qualified Windows.Environment as Env

import Prompt
import PromptMessage

data Options = Options
    { optYes    :: Bool
    , optGlobal :: Bool
    } deriving (Eq, Show)

optionParser :: Parser Options
optionParser = Options
    <$> optYesDesc
    <*> optGlobalDesc
  where
    optYesDesc = switch
         $ long "yes" <> short 'y'
        <> help "Skip confirmation prompt"
    optGlobalDesc = switch
         $ long "global" <> short 'g'
        <> help "Set up for all users"

data Dirs = Dirs
    { pdbsDir    :: String
    , symbolsDir :: String
    } deriving (Eq, Show)

dirPaths :: Dirs -> [String]
dirPaths dirs = [ pdbsDir dirs
                , symbolsDir dirs
                ]

createDirs :: Dirs -> IO ()
createDirs = mapM_ (createDirectoryIfMissing True) . dirPaths

toRemoteDirs :: Dirs -> Dirs
toRemoteDirs localDirs = localDirs
    { symbolsDir = remoteSymbolsDir $ symbolsDir localDirs
    }
  where
    remoteSymbolsDir localDir = "SRV*" ++ localDir ++ "*" ++ remoteSymbolsUrl
    remoteSymbolsUrl = "http://msdl.microsoft.com/download/symbols"

getLocalDirs :: IO Dirs
getLocalDirs = do
    cwd <- getCurrentDirectory
    return Dirs
        { pdbsDir    = combine cwd "pdbs"
        , symbolsDir = combine cwd "symbols"
        }

fixNtSymbolPath :: Options -> IO ()
fixNtSymbolPath options = do
    oldValue <- Env.query profile varName
    let oldPaths = Env.pathSplit $ fromMaybe "" oldValue
    localDirs <- getLocalDirs
    let remoteDirs = toRemoteDirs localDirs
    let newPaths = union oldPaths $ dirPaths remoteDirs
    when (length oldPaths /= length newPaths) $ do
        let newValue = Env.pathJoin newPaths
        let promptAnd = if skipPrompt
            then withoutPrompt
            else withPrompt $ engraveMessage profile varName oldValue newValue
        let engrave = Env.engrave profile varName newValue
        agreed <- promptAnd engrave
        when agreed $
            createDirs localDirs
  where
    varName = "_NT_SYMBOL_PATH"

    forAllUsers = optGlobal options
    profile
        | forAllUsers = Env.AllUsers
        | otherwise   = Env.CurrentUser

    skipPrompt = optYes options

main :: IO ()
main = execParser parser >>= fixNtSymbolPath
  where
    parser = info (helper <*> optionParser) $
        fullDesc <> progDesc "Set up your _NT_SYMBOL_PATH"

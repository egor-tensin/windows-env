{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (when)
import Data.List     ((\\))
import Data.Maybe    (fromJust, isJust)

import Options.Applicative

import qualified Environment

data Options = Options
    { optName   :: String
    , optYes    :: Bool
    , optGlobal :: Bool
    , optPaths  :: [String]
    } deriving (Eq, Show)

options = Options
    <$> optNameDesc
    <*> optYesDesc
    <*> optGlobalDesc
    <*> optPathsDesc
  where
    optNameDesc = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Variable name ('PATH' by default)"
    optYesDesc = switch $
        long "yes" <> short 'y' <>
        help "Skip confirmation prompt"
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Remove for all users"
    optPathsDesc = many $ argument str $
        metavar "PATH" <>
        help "Directories to remove"

main :: IO ()
main = execParser parser >>= removePath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Remove directories from your PATH"

removePath :: Options -> IO ()
removePath options = do
    removePathFrom Environment.CurrentUser
    when forAllUsers $ do
        removePathFrom Environment.AllUsers
  where
    varName = optName options
    pathsToRemove = optPaths options

    forAllUsers = optGlobal options

    removePathFrom env = do
        oldValue <- Environment.query env varName
        when (isJust oldValue) $ do
            let oldPaths = Environment.pathSplit $ fromJust oldValue
            let newPaths = oldPaths \\ pathsToRemove
            when (length oldPaths /= length newPaths) $ do
                let newValue = Environment.pathJoin newPaths
                engrave env newValue

    skipPrompt = optYes options
    engrave env value = if skipPrompt
        then Environment.engrave       env varName value
        else Environment.engravePrompt env varName value >> return ()

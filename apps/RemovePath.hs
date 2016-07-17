{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (void, when)
import Data.List     ((\\))
import Data.Maybe    (fromJust, isJust)

import           Options.Applicative
import qualified Windows.Environment as Env

import Banner
import Prompt

data Options = Options
    { optName   :: Env.VarName
    , optYes    :: Bool
    , optGlobal :: Bool
    , optPaths  :: [Env.VarValue]
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
    removePathFrom Env.CurrentUser
    when forAllUsers $ do
        removePathFrom Env.AllUsers
  where
    varName = optName options
    pathsToRemove = optPaths options

    forAllUsers = optGlobal options

    removePathFrom profile = do
        oldValue <- Env.query profile varName
        when (isJust oldValue) $ do
            let oldPaths = Env.pathSplit $ fromJust oldValue
            let newPaths = oldPaths \\ pathsToRemove
            when (length oldPaths /= length newPaths) $ do
                let newValue = Env.pathJoin newPaths
                let banner = engraveBanner profile varName oldValue newValue
                void $ prompt banner $ Env.engrave profile varName newValue

    skipPrompt = optYes options
    prompt
        | skipPrompt = const withoutPrompt
        | otherwise  = withPrompt

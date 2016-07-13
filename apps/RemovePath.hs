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
    { optName :: String
    , optGlobal :: Bool
    , optPaths :: [String]
    } deriving (Eq, Show)

options = Options
    <$> optNameDesc
    <*> optGlobalDesc
    <*> optPathsDesc
  where
    optNameDesc = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Specify variable name ('PATH' by default)"
    optGlobalDesc = switch $
        long "global" <> short 'g' <>
        help "Whether to remove for all users"
    optPathsDesc = many $ argument str $
        metavar "PATH" <>
        help "Directory path(s)"

main :: IO ()
main = execParser parser >>= removePath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Remove directories from your PATH"

removePath :: Options -> IO ()
removePath options = do
    removePathFrom Environment.CurrentUser options
    when (optGlobal options) $ do
        removePathFrom Environment.AllUsers options
  where
    varName = optName options
    pathsToRemove = optPaths options

    removePathFrom env options = do
        oldValue <- Environment.query env varName
        when (isJust oldValue) $ do
            let oldPaths = Environment.pathSplit $ fromJust oldValue
            let newPaths = oldPaths \\ pathsToRemove
            when (length oldPaths /= length newPaths) $ do
                let newValue = Environment.pathJoin newPaths
                Environment.engraveWithPrompt env varName newValue

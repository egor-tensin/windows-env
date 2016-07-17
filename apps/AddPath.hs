{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (void, when)
import Data.List     (union)
import Data.Maybe    (fromMaybe)
import Text.Printf   (printf)

import           Options.Applicative
import qualified Windows.Environment as Env

import qualified Utils

data Options = Options
    { optName   :: Env.VarName
    , optYes    :: Bool
    , optGlobal :: Bool
    , optPaths  :: [Env.VarValue]
    } deriving (Eq, Show)

options :: Parser Options
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
        help "Add for all users"
    optPathsDesc = many $ argument str $
        metavar "PATH" <>
        help "Directories to add"

main :: IO ()
main = execParser parser >>= addPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Add directories to your PATH"

addPath :: Options -> IO ()
addPath options = do
    oldValue <- Env.query profile varName
    let oldPaths = Env.pathSplit $ fromMaybe "" oldValue
    let newPaths = union oldPaths pathsToAdd
    when (length oldPaths /= length newPaths) $ do
        let newValue = Env.pathJoin newPaths
        let promptBanner = Utils.engraveBanner profile varName oldValue newValue
        void $ prompt promptBanner $ Env.engrave profile varName newValue
  where
    varName = optName options
    pathsToAdd = optPaths options

    forAllUsers = optGlobal options
    profile = if forAllUsers
        then Env.AllUsers
        else Env.CurrentUser

    skipPrompt = optYes options
    prompt = if skipPrompt
        then const Utils.withoutPrompt
        else Utils.withPrompt

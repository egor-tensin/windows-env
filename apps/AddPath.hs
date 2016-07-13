{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (when)
import Data.List     (union)
import Data.Maybe    (fromMaybe)

import Options.Applicative

import qualified Environment

data Options = Options
    { optName   :: String
    , optGlobal :: Bool
    , optPaths  :: [String]
    } deriving (Eq, Show)

options :: Parser Options
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
        help "Whether to add for all users"
    optPathsDesc = many $ argument str $
        metavar "PATH" <>
        help "Directory path(s)"

main :: IO ()
main = execParser parser >>= addPath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Add directories to your PATH"

addPath :: Options -> IO ()
addPath options = do
    oldValue <- Environment.query env varName
    let oldPaths = Environment.pathSplit $ fromMaybe "" oldValue
    let newPaths = union oldPaths pathsToAdd
    when (length oldPaths /= length newPaths) $ do
        let newValue = Environment.pathJoin newPaths
        Environment.engraveWithPrompt env varName newValue
  where
    env | optGlobal options = Environment.AllUsers
        | otherwise         = Environment.CurrentUser
    varName = optName options
    pathsToAdd = optPaths options

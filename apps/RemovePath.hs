{-
 - Copyright 2015 Egor Tensin <Egor.Tensin@gmail.com>
 - This file is licensed under the terms of the MIT License.
 - See LICENSE.txt for details.
-}

module Main (main) where

import Control.Monad (when)

import Options.Applicative

import qualified Environment

data Options = Options
    { name :: String
    , global :: Bool
    , paths :: [String]
    } deriving (Eq, Show)

options = Options
    <$> nameOption
    <*> globalOption
    <*> pathArgs
  where
    nameOption = strOption $
        long "name" <> short 'n' <> metavar "NAME" <> value "PATH" <>
        help "Specify variable name ('PATH' by default)"
    globalOption = switch $
        long "global" <> short 'g' <>
        help "Whether to remove for all users"
    pathArgs = many $ argument str $
        metavar "PATH" <>
        help "Directory path(s)"

main :: IO ()
main = execParser parser >>= removePath
  where
    parser = info (helper <*> options) $
        fullDesc <> progDesc "Remove directories from your PATH"

removePath :: Options -> IO ()
removePath options = do
    let varName = name options
    userVal <- Environment.query Environment.CurrentUser varName
    let userValParts = Environment.pathSplit userVal
    let newUserValParts = filter (flip notElem $ paths options) userValParts
    when (length userValParts /= length newUserValParts) $ do
        Environment.engraveWithPrompt Environment.CurrentUser varName $ Environment.pathJoin newUserValParts
    when (global options) $ do
        globalVal <- Environment.query Environment.AllUsers varName
        let globalValParts = Environment.pathSplit globalVal
        let newGlobalValParts = filter (flip notElem $ paths options) globalValParts
        when (length globalValParts /= length newGlobalValParts) $ do
            Environment.engraveWithPrompt Environment.AllUsers varName $ Environment.pathJoin newGlobalValParts

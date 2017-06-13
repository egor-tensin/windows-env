-- |
-- Copyright   : (c) 2017 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
-- Portability : Windows-only

module Utils.Path
    ( ExpandedPath(..)
    , pathExpandValue

    , pathExpandAll
    , pathAnyExpanded
    ) where

import Control.Monad.Trans.Except (ExceptT)

import qualified WindowsEnv

data ExpandedPath = ExpandedPath
    { pathOriginal :: String
    , pathExpanded :: String
    } deriving (Eq, Show)

pathExpandValue :: WindowsEnv.Value -> ExceptT IOError IO [ExpandedPath]
pathExpandValue value
    | WindowsEnv.valueExpandable value = do
        expanded <- expandOnce
        zipWith ExpandedPath split <$>
            if length expanded == length split
                then return expanded
                else expandEach
    | otherwise = return $ zipWith ExpandedPath split split
  where
    joined = WindowsEnv.valueString value
    split = WindowsEnv.pathSplit joined
    expandOnce = WindowsEnv.pathSplit <$> WindowsEnv.expand joined
    expandEach = WindowsEnv.expandAll split

pathExpandAll :: [String] -> ExceptT IOError IO [ExpandedPath]
pathExpandAll paths = zipWith ExpandedPath paths <$> WindowsEnv.expandAll paths

pathIsExpanded :: ExpandedPath -> Bool
pathIsExpanded path = pathOriginal path /= pathExpanded path

pathAnyExpanded :: [ExpandedPath] -> Bool
pathAnyExpanded = any pathIsExpanded

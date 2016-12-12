-- |
-- Copyright   : (c) 2015 Egor Tensin <Egor.Tensin@gmail.com>
-- License     : MIT
-- Maintainer  : Egor.Tensin@gmail.com
-- Stability   : experimental
--
-- High-level functions for reading and writing Windows environment variables.

module Windows.Environment
    ( Profile(..)
    , profileKeyPath

    , VarName
    , VarValue
    , query
    , engrave
    , wipe

    , pathJoin
    , pathSplit
    ) where

import Control.Exception (finally)
import Data.List         (intercalate)
import Data.List.Split   (splitOn)

import qualified Windows.Registry as Registry
import           Windows.Utils    (notifyEnvironmentUpdate)

data Profile = CurrentUser
             | AllUsers
             deriving (Eq, Show)

profileKeyPath :: Profile -> Registry.KeyPath
profileKeyPath CurrentUser = Registry.KeyPath Registry.CurrentUser ["Environment"]
profileKeyPath AllUsers    = Registry.KeyPath Registry.LocalMachine
    [ "SYSTEM"
    , "CurrentControlSet"
    , "Control"
    , "Session Manager"
    , "Environment"
    ]

type VarName  = String
type VarValue = String

query :: Profile -> VarName -> IO (Either IOError VarValue)
query profile name = Registry.getExpandedString (profileKeyPath profile) name

engrave :: Profile -> VarName -> VarValue -> IO (Either IOError ())
engrave profile name value = finally doEngrave notifyEnvironmentUpdate
  where
    doEngrave = Registry.setStringPreserveType (profileKeyPath profile) name value

wipe :: Profile -> VarName -> IO (Either IOError ())
wipe profile name = finally doWipe notifyEnvironmentUpdate
  where
    doWipe = Registry.deleteValue (profileKeyPath profile) name

pathSep :: VarValue
pathSep = ";"

pathSplit :: VarValue -> [VarValue]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [VarValue] -> VarValue
pathJoin = intercalate pathSep . filter (not . null)

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
    , engraveForce
    , wipe

    , pathJoin
    , pathSplit
    ) where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.List       (intercalate)
import Data.List.Split (splitOn)

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

query :: Profile -> VarName -> ExceptT IOError IO VarValue
query profile name = Registry.getExpandedString (profileKeyPath profile) name

engrave :: Profile -> VarName -> VarValue -> ExceptT IOError IO ()
engrave profile name value = do
    ret <- Registry.setStringPreserveType (profileKeyPath profile) name value
    lift notifyEnvironmentUpdate
    return ret

engraveForce :: Profile -> VarName -> VarValue -> ExceptT IOError IO ()
engraveForce profile name value = do
    ret <- Registry.setString (profileKeyPath profile) name value
    lift notifyEnvironmentUpdate
    return ret

wipe :: Profile -> VarName -> ExceptT IOError IO ()
wipe profile name = do
    ret <- Registry.deleteValue (profileKeyPath profile) name
    lift notifyEnvironmentUpdate
    return ret

pathSep :: VarValue
pathSep = ";"

pathSplit :: VarValue -> [VarValue]
pathSplit = filter (not . null) . splitOn pathSep

pathJoin :: [VarValue] -> VarValue
pathJoin = intercalate pathSep . filter (not . null)

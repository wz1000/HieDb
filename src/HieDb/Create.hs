{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module HieDb.Create where

import Prelude hiding (mod)

import GHC
import HieTypes

import Control.Monad.IO.Class

import System.Directory

import Database.SQLite.Simple
import Data.Time.Clock

import HieDb.Types
import HieDb.Utils

withHieDb :: FilePath -> (HieDb -> IO a) -> IO a
withHieDb fp f = withConnection fp (f . HieDb)

initConn :: HieDb -> IO ()
initConn (getConn -> conn) = do
  execute_ conn "CREATE TABLE IF NOT EXISTS refs (src TEXT, srcMod TEXT, srcUnit TEXT, occ TEXT, mod TEXT, unit TEXT, file TEXT, sl INTEGER, sc INTEGER, el INTEGER, ec INTEGER)"
  execute_ conn "CREATE TABLE IF NOT EXISTS mods (hieFile TEXT PRIMARY KEY ON CONFLICT REPLACE, mod TEXT, unit TEXT, time TEXT, CONSTRAINT modid UNIQUE (mod, unit) ON CONFLICT REPLACE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS decls (hieFile TEXT, mod TEXT, unit TEXT, occ TEXT, file TEXT, sl INTEGER, sc INTEGER, el INTEGER, ec INTEGER, is_root BOOL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS defs (hieFile TEXT, mod TEXT, unit TEXT, occ TEXT, file TEXT, sl INTEGER, sc INTEGER, el INTEGER, ec INTEGER)"

addRefsFrom :: (MonadIO m, NameCacheMonad m) => HieDb -> FilePath -> m ()
addRefsFrom c@(getConn -> conn) path = do
  time <- liftIO $ getModificationTime path
  mods <- liftIO $ query conn "SELECT * FROM mods WHERE hieFile = ? AND time >= ?" (path, time)
  case mods of
    (HieModuleRow{}:_) -> return ()
    [] -> withHieFile path $ \hf -> addRefsFromLoaded c path time hf

addRefsFromLoaded :: (MonadIO m) => HieDb -> FilePath -> UTCTime -> HieFile -> m ()
addRefsFromLoaded (getConn -> conn) path time hf = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM refs WHERE src = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs WHERE hieFile = ?" (Only path)
  let mod = moduleName $ hie_module hf
      uid = moduleUnitId $ hie_module hf
      modrow = HieModuleRow path mod uid time
  execute conn "INSERT INTO mods VALUES (?,?,?,?)" modrow
  let rows = genRefRow path hf
  executeMany conn "INSERT INTO refs VALUES (?,?,?,?,?,?,?,?,?,?,?)" rows
  let decls = genDeclRow path hf
  executeMany conn "INSERT INTO decls VALUES (?,?,?,?,?,?,?,?,?,?)" decls
  let defs = genDefRow path hf
  executeMany conn "INSERT INTO defs VALUES (?,?,?,?,?,?,?,?,?)" defs

deleteFileFromIndex :: HieDb -> FilePath -> IO ()
deleteFileFromIndex (getConn -> conn) path = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM mods WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM refs WHERE src = ?" (Only path)

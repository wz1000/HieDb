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
import HieUtils

import Control.Monad.IO.Class
import Control.Exception

import System.Directory

import Database.SQLite.Simple
import Data.Time.Clock
import Data.List ( isSuffixOf )
import Data.String

import HieDb.Types
import HieDb.Utils

sCHEMA_VERSION :: Integer
sCHEMA_VERSION = 1

dB_VERSION :: Integer
dB_VERSION = read (show sCHEMA_VERSION ++ "999" ++ show hieVersion)


checkVersion :: (HieDb -> IO a) -> (HieDb -> IO a)
checkVersion k db@(getConn -> conn) = do
  [Only ver] <- query_ conn "PRAGMA user_version"
  if ver == 0 then do
    execute_ conn $ fromString $ "PRAGMA user_version = " ++ show dB_VERSION
    k db
  else if ver == dB_VERSION then do
    k db
  else
    throwIO $ IncompatibleSchemaVersion dB_VERSION ver

withHieDb :: FilePath -> (HieDb -> IO a) -> IO a
withHieDb fp f = withConnection fp (checkVersion f . HieDb)

initConn :: HieDb -> IO ()
initConn (getConn -> conn) = do
  execute_ conn "PRAGMA journal_mode = WAL;"
  execute_ conn "PRAGMA foreign_keys = ON;"

  execute_ conn "CREATE TABLE IF NOT EXISTS mods \
                \( hieFile TEXT NOT NULL PRIMARY KEY ON CONFLICT REPLACE \
                \, mod     TEXT NOT NULL \
                \, unit    TEXT NOT NULL \
                \, is_boot BOOL NOT NULL \
                \, hs_src  TEXT UNIQUE ON CONFLICT REPLACE \
                \, time    TEXT NOT NULL \
                \, CONSTRAINT modid UNIQUE (mod, unit, is_boot) ON CONFLICT REPLACE \
                \)"

  execute_ conn "CREATE TABLE IF NOT EXISTS refs \
                \( hieFile TEXT NOT NULL \
                \, occ     TEXT NOT NULL \
                \, mod     TEXT NOT NULL \
                \, unit    TEXT NOT NULL \
                \, file    TEXT NOT NULL \
                \, sl   INTEGER NOT NULL \
                \, sc   INTEGER NOT NULL \
                \, el   INTEGER NOT NULL \
                \, ec   INTEGER NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) \
                \)"

  execute_ conn "CREATE TABLE IF NOT EXISTS decls \
                \( hieFile    TEXT NOT NULL \
                \, occ        TEXT NOT NULL \
                \, file       TEXT NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, is_root    BOOL NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) \
                \)"

  execute_ conn "CREATE TABLE IF NOT EXISTS defs \
                \( hieFile    TEXT NOT NULL \
                \, occ        TEXT NOT NULL \
                \, file       TEXT NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) \
                \, PRIMARY KEY(hieFile,occ) \
                \)"

addRefsFrom :: (MonadIO m, NameCacheMonad m) => HieDb -> FilePath -> m ()
addRefsFrom c@(getConn -> conn) path = do
  time <- liftIO $ getModificationTime path
  mods <- liftIO $ query conn "SELECT * FROM mods WHERE hieFile = ? AND time >= ?" (path, time)
  let isBoot = "boot" `isSuffixOf` path
  case mods of
    (HieModuleRow{}:_) -> return ()
    [] -> withHieFile path $ \hf -> addRefsFromLoaded c path isBoot Nothing time hf

addRefsFromLoaded :: (MonadIO m) => HieDb -> FilePath -> Bool -> Maybe FilePath -> UTCTime -> HieFile -> m ()
addRefsFromLoaded (getConn -> conn) path isBoot srcFile time hf = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM refs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs  WHERE hieFile = ?" (Only path)

  let mod    = moduleName smod
      uid    = moduleUnitId smod
      smod   = hie_module hf
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
      modrow = HieModuleRow path (ModuleInfo mod uid isBoot srcFile time)

  execute conn "INSERT INTO mods VALUES (?,?,?,?,?,?)" modrow

  let (rows,decls) = genRefsAndDecls path smod refmap
  executeMany conn "INSERT INTO refs  VALUES (?,?,?,?,?,?,?,?,?)" rows
  executeMany conn "INSERT INTO decls VALUES (?,?,?,?,?,?,?,?)" decls

  let defs = genDefRow path smod refmap
  executeMany conn "INSERT INTO defs VALUES (?,?,?,?,?,?,?)" defs

deleteFileFromIndex :: HieDb -> FilePath -> IO ()
deleteFileFromIndex (getConn -> conn) path = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM mods  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM refs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs  WHERE hieFile = ?" (Only path)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module HieDb.Create where

import Prelude hiding (mod)

import GHC
import Compat.HieTypes
import Compat.HieUtils
import IfaceType
import Name

import Control.Monad.IO.Class
import Control.Monad
import Control.Exception

import System.Directory

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ (sql)
import Data.Time.Clock
import Data.List ( isSuffixOf )
import Data.String
import Data.Int

import HieDb.Types
import HieDb.Utils
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe

sCHEMA_VERSION :: Integer
sCHEMA_VERSION = 2

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
withHieDb fp f = withConnection fp (checkVersion f . flip HieDb Nothing)

withHieDb' :: FilePath -> FilePath -> (HieDb -> IO a) -> IO a
withHieDb' libdir fp f = do
  df <- dynFlagsForPrinting libdir
  withConnection fp (checkVersion f . flip HieDb (Just df))

initConn :: HieDb -> IO ()
initConn (getConn -> conn) = do
  execute_ conn "PRAGMA journal_mode = WAL;"
  execute_ conn "PRAGMA foreign_keys = ON;"
  execute_ conn "PRAGMA defer_foreign_keys = ON;"

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS mods
    ( hieFile TEXT NOT NULL PRIMARY KEY ON CONFLICT REPLACE
    , mod     TEXT NOT NULL
    , unit    TEXT NOT NULL
    , is_boot BOOL NOT NULL
    , hs_src  TEXT UNIQUE ON CONFLICT REPLACE
    , is_real BOOL NOT NULL
    , time    TEXT NOT NULL
    , CONSTRAINT modid UNIQUE (mod, unit, is_boot) ON CONFLICT REPLACE
    )
  |]

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS refs
    ( hieFile TEXT    NOT NULL
    , occ     TEXT    NOT NULL
    , mod     TEXT    NOT NULL
    , unit    TEXT    NOT NULL
    , file    TEXT    NOT NULL
    , sl      INTEGER NOT NULL
    , sc      INTEGER NOT NULL
    , el      INTEGER NOT NULL
    , ec      INTEGER NOT NULL
    , FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
    )
  |]

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS decls
    ( hieFile    TEXT    NOT NULL
    , occ        TEXT    NOT NULL
    , file       TEXT    NOT NULL
    , sl         INTEGER NOT NULL
    , sc         INTEGER NOT NULL
    , el         INTEGER NOT NULL
    , ec         INTEGER NOT NULL
    , is_root    BOOL    NOT NULL
    , FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
    )
  |]

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS defs
    ( hieFile    TEXT    NOT NULL
    , occ        TEXT    NOT NULL
    , file       TEXT    NOT NULL
    , sl         INTEGER NOT NULL
    , sc         INTEGER NOT NULL
    , el         INTEGER NOT NULL
    , ec         INTEGER NOT NULL
    , type       TEXT            
    , docs       TEXT            
    , FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
    , PRIMARY KEY(hieFile,occ)
    )
  |]

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS typenames
    ( id         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
    , name       TEXT    NOT NULL
    , mod        TEXT    NOT NULL
    , unit       TEXT    NOT NULL
    , CONSTRAINT uniqname UNIQUE (name, mod, unit) ON CONFLICT IGNORE
    )
  |]

  execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS typerefs
    ( id      INTEGER NOT NULL
    , hieFile TEXT    NOT NULL
    , depth   INTEGER NOT NULL
    , file    TEXT    NOT NULL
    , sl      INTEGER NOT NULL
    , sc      INTEGER NOT NULL
    , el      INTEGER NOT NULL
    , ec      INTEGER NOT NULL
    , FOREIGN KEY(id) REFERENCES typenames(id) DEFERRABLE INITIALLY DEFERRED
    , FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
    )
  |]

addArr :: HieDb -> A.Array TypeIndex HieTypeFlat -> IO (A.Array TypeIndex (Maybe Int64))
addArr (getConn -> conn) arr = do
  fmap (A.listArray (A.bounds arr)) $ forM (A.elems arr) $ \case
    HTyVarTy n -> addName n
    HTyConApp tc _ -> addName (ifaceTyConName tc)
    _ -> pure Nothing
  where
    addName :: Name -> IO (Maybe Int64)
    addName n = case nameModule_maybe n of
      Nothing -> pure Nothing
      Just m -> liftIO $ do
        let occ = nameOccName n
            mod = moduleName m
            uid = moduleUnitId m
        execute conn "INSERT INTO typenames(name,mod,unit) VALUES (?,?,?)" (occ,mod,uid)
        Just . fromOnly . head <$> query conn "SELECT id FROM typenames WHERE name = ? AND mod = ? AND unit = ?" (occ,mod,uid)

addTypeRefs :: HieDb -> FilePath -> HieFile -> A.Array TypeIndex (Maybe Int64) -> IO ()
addTypeRefs db path hf ixs = mapM_ addTypesFromAst asts
  where
    arr = hie_types hf
    asts = M.elems $ getAsts $ hie_asts hf
    addTypesFromAst :: HieAST TypeIndex -> IO ()
    addTypesFromAst ast = do
      mapM_ (addTypeRef db path arr ixs (nodeSpan ast)) $ mapMaybe identType $ M.elems $ nodeIdentifiers $ nodeInfo ast
      when (null $ nodeChildren ast) $
        mapM_ (addTypeRef db path arr ixs (nodeSpan ast)) $ nodeType $ nodeInfo ast
      mapM_ addTypesFromAst $ nodeChildren ast

addRefsFrom :: (MonadIO m, NameCacheMonad m) => HieDb -> FilePath -> m ()
addRefsFrom c@(getConn -> conn) path = do
  time <- liftIO $ getModificationTime path
  mods <- liftIO $ query conn "SELECT * FROM mods WHERE hieFile = ? AND time >= ?" (path, time)
  let isBoot = "boot" `isSuffixOf` path
  case mods of
    (HieModuleRow{}:_) -> return ()
    [] -> withHieFile path $ \hf -> addRefsFromLoaded c path isBoot Nothing False time hf emptyDeclDocMap

addRefsFromLoaded :: (MonadIO m) => HieDb -> FilePath -> Bool -> Maybe FilePath -> Bool -> UTCTime -> HieFile -> DeclDocMap -> m ()
addRefsFromLoaded db@(getConn -> conn) path isBoot srcFile isReal time hf docs = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM refs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM typerefs WHERE hieFile = ?" (Only path)

  let mod    = moduleName smod
      uid    = moduleUnitId smod
      smod   = hie_module hf
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
      modrow = HieModuleRow path (ModuleInfo mod uid isBoot srcFile isReal time)

  execute conn "INSERT INTO mods VALUES (?,?,?,?,?,?,?)" modrow

  let (rows,decls) = genRefsAndDecls path smod refmap
  executeMany conn "INSERT INTO refs  VALUES (?,?,?,?,?,?,?,?,?)" rows
  executeMany conn "INSERT INTO decls VALUES (?,?,?,?,?,?,?,?)" decls

  ixs <- addArr db (hie_types hf)

  let printType = case getDbDynFlags db of
        Just df -> renderHieType df . flip recoverFullType (hie_types hf)
        Nothing -> const ""
  let defs = genDefRow path smod docs refmap printType
  executeMany conn "INSERT INTO defs VALUES (?,?,?,?,?,?,?,?,?)" defs

  when isReal $
    addTypeRefs db path hf ixs

addSrcFile :: HieDb -> FilePath -> FilePath -> Bool -> IO ()
addSrcFile (getConn -> conn) hie srcFile isReal=
  execute conn "UPDATE mods SET hs_src = ? , is_real = ? WHERE hieFile = ?" (srcFile, isReal, hie)

deleteFileFromIndex :: HieDb -> FilePath -> IO ()
deleteFileFromIndex (getConn -> conn) path = liftIO $ withTransaction conn $ do
  execute conn "DELETE FROM mods  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM refs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs  WHERE hieFile = ?" (Only path)

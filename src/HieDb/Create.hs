{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module HieDb.Create where

import Prelude hiding (mod)

import Compat.HieTypes
import Compat.HieUtils

import GHC

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Array as A
import qualified Data.Map as M

import Data.Int
import Data.List ( isSuffixOf )
import Data.Maybe
import Data.String

import System.Directory
import System.FilePath

import Database.SQLite.Simple

import HieDb.Compat as Compat
import HieDb.Types
import HieDb.Utils

sCHEMA_VERSION :: Integer
sCHEMA_VERSION = 7

dB_VERSION :: Integer
dB_VERSION = read (show sCHEMA_VERSION ++ "999" ++ show hieVersion)

{-| @checkVersion f db@ checks the schema version associated with given @db@.
If that version is supported by hiedb, it runs the function @f@ with the @db@.
Otherwise it throws 'IncompatibleSchemaVersion' exception.
-}
checkVersion :: (HieDb -> IO a) -> HieDb -> IO a
checkVersion k db@(getConn -> conn) = do
  execute_ conn "PRAGMA busy_timeout = 500;"
  execute_ conn "PRAGMA journal_mode = WAL;"
  [Only ver] <- query_ conn "PRAGMA user_version"
  if ver == 0 then do
    execute_ conn $ fromString $ "PRAGMA user_version = " ++ show dB_VERSION
    k db
  else if ver == dB_VERSION then do
    k db
  else
    throwIO $ IncompatibleSchemaVersion dB_VERSION ver

{-| Given path to @.hiedb@ file, constructs 'HieDb' and passes it to given function. -}
withHieDb :: FilePath -> (HieDb -> IO a) -> IO a
withHieDb fp f = withConnection fp (checkVersion f . HieDb)

{-| Given GHC LibDir and path to @.hiedb@ file,
constructs DynFlags (required for printing info from @.hie@ files)
and 'HieDb' and passes them to given function.
-}
withHieDbAndFlags :: LibDir -> FilePath -> (DynFlags -> HieDb -> IO a) -> IO a
withHieDbAndFlags libdir fp f = do
  dynFlags <- dynFlagsForPrinting libdir
  withConnection fp (checkVersion (f dynFlags) . HieDb)

{-| Initialize database schema for given 'HieDb'.
-}
initConn :: HieDb -> IO ()
initConn (getConn -> conn) = do
  execute_ conn "PRAGMA busy_timeout = 500;"
  execute_ conn "PRAGMA journal_mode = WAL;"
  execute_ conn "PRAGMA foreign_keys = ON;"
  execute_ conn "PRAGMA defer_foreign_keys = ON;"

  execute_ conn "CREATE TABLE IF NOT EXISTS mods \
                \( hieFile TEXT NOT NULL PRIMARY KEY ON CONFLICT REPLACE \
                \, mod     TEXT NOT NULL \
                \, unit    TEXT NOT NULL \
                \, is_boot BOOL NOT NULL \
                \, hs_src  TEXT UNIQUE ON CONFLICT REPLACE \
                \, is_real BOOL NOT NULL \
                \, hash    TEXT NOT NULL UNIQUE ON CONFLICT REPLACE \
                \, CONSTRAINT modid UNIQUE (mod, unit, is_boot) ON CONFLICT REPLACE \
                \, CONSTRAINT real_has_src CHECK ( (NOT is_real) OR (hs_src IS NOT NULL) ) \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS mod_hash ON mods(hieFile,hash)"
  execute_ conn "CREATE INDEX IF NOT EXISTS mod_unit ON mods(unit)"

  execute_ conn "CREATE TABLE IF NOT EXISTS exports \
                \( hieFile TEXT NOT NULL \
                \, occ     TEXT NOT NULL \
                \, mod     TEXT NOT NULL \
                \, unit    TEXT NOT NULL \
                \, parent  TEXT \
                \, parentMod TEXT \
                \, parentUnit TEXT \
                \, is_datacon BOOL NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS exports_mod ON exports(hieFile)"

  execute_ conn "CREATE TABLE IF NOT EXISTS refs \
                \( hieFile TEXT NOT NULL \
                \, occ     TEXT NOT NULL \
                \, mod     TEXT NOT NULL \
                \, unit    TEXT NOT NULL \
                \, sl   INTEGER NOT NULL \
                \, sc   INTEGER NOT NULL \
                \, el   INTEGER NOT NULL \
                \, ec   INTEGER NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS refs_mod ON refs(hieFile)"

  execute_ conn "CREATE TABLE IF NOT EXISTS decls \
                \( hieFile    TEXT NOT NULL \
                \, occ        TEXT NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, is_root    BOOL NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS decls_mod ON decls(hieFile)"

  execute_ conn "CREATE TABLE IF NOT EXISTS imports \
                \( hieFile    TEXT NOT NULL \
                \, mod     TEXT NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS imports_mod ON imports(mod)"
  execute_ conn "CREATE INDEX IF NOT EXISTS imports_hiefile ON imports(hieFile)"


  execute_ conn "CREATE TABLE IF NOT EXISTS defs \
                \( hieFile    TEXT NOT NULL \
                \, occ        TEXT NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \, PRIMARY KEY(hieFile,occ) \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS defs_mod ON defs(hieFile)"

  execute_ conn "CREATE TABLE IF NOT EXISTS typenames \
                \( id      INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
                \, name       TEXT NOT NULL \
                \, mod        TEXT NOT NULL \
                \, unit       TEXT NOT NULL \
                \, CONSTRAINT uniqname UNIQUE (name, mod, unit) ON CONFLICT IGNORE \
                \)"

  execute_ conn "CREATE TABLE IF NOT EXISTS typerefs \
                \( id   INTEGER NOT NULL \
                \, hieFile    TEXT NOT NULL \
                \, depth   INTEGER NOT NULL \
                \, sl      INTEGER NOT NULL \
                \, sc      INTEGER NOT NULL \
                \, el      INTEGER NOT NULL \
                \, ec      INTEGER NOT NULL \
                \, FOREIGN KEY(id) REFERENCES typenames(id) DEFERRABLE INITIALLY DEFERRED \
                \, FOREIGN KEY(hieFile) REFERENCES mods(hieFile) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED \
                \)"
  execute_ conn "CREATE INDEX IF NOT EXISTS typeref_id ON typerefs(id)"
  execute_ conn "CREATE INDEX IF NOT EXISTS typerefs_mod ON typerefs(hieFile)"

{-| Add names of types from @.hie@ file to 'HieDb'.
Returns an Array mapping 'TypeIndex' to database ID assigned to the
corresponding record in DB.
-}
addArr :: HieDb -> A.Array TypeIndex HieTypeFlat -> IO (A.Array TypeIndex (Maybe Int64))
addArr (getConn -> conn) arr = do
  forM arr $ \case
    HTyVarTy n -> addName n
    HTyConApp tc _ -> addName (ifaceTyConName tc)
    _ -> pure Nothing
  where
    addName :: Name -> IO (Maybe Int64)
    addName n = case nameModule_maybe n of
      Nothing -> pure Nothing
      Just m -> do
        let occ = nameOccName n
            mod = moduleName m
            uid = moduleUnit m
        execute conn "INSERT INTO typenames(name,mod,unit) VALUES (?,?,?)" (occ,mod,uid)
        fmap fromOnly . listToMaybe <$> query conn "SELECT id FROM typenames WHERE name = ? AND mod = ? AND unit = ?" (occ,mod,uid)

{-| Add references to types from given @.hie@ file to DB. -}
addTypeRefs
  :: HieDb
  -> FilePath -- ^ Path to @.hie@ file
  -> HieFile -- ^ Data loaded from the @.hie@ file
  -> A.Array TypeIndex (Maybe Int64) -- ^ Maps TypeIndex to database ID assigned to record in @typenames@ table
  -> IO ()
addTypeRefs db path hf ixs = mapM_ addTypesFromAst asts
  where
    arr :: A.Array TypeIndex HieTypeFlat
    arr = hie_types hf
    asts :: M.Map HiePath (HieAST TypeIndex)
    asts = getAsts $ hie_asts hf
    addTypesFromAst :: HieAST TypeIndex -> IO ()
    addTypesFromAst ast = do
      mapM_ (addTypeRef db path arr ixs (nodeSpan ast))
        $ mapMaybe (\x -> guard (not (all isOccurrence (identInfo x))) *> identType x)
        $ M.elems
        $ nodeIdentifiers
        $ nodeInfo' ast
      mapM_ addTypesFromAst $ nodeChildren ast

-- | Options to skip indexing phases
data SkipOptions =
  SkipOptions
    {
    skipRefs :: Bool
    , skipDecls :: Bool
    , skipDefs :: Bool
    , skipExports :: Bool
    , skipImports :: Bool
    , skipTypes :: Bool
    -- ^ Note skip types will also skip type refs since it is dependent
    , skipTypeRefs :: Bool
    }
    deriving Show

defaultSkipOptions :: SkipOptions
defaultSkipOptions =
  SkipOptions
    {
    skipRefs = False
    , skipDecls = False
    , skipDefs = False
    , skipExports = False
    , skipImports = False
    , skipTypes = False
    -- ^ Note skip types will also skip type refs since it is dependent
    , skipTypeRefs = False
    }

{-| Adds all references from given @.hie@ file to 'HieDb'.
The indexing is skipped if the file was not modified since the last time it was indexed.
The boolean returned is true if the file was actually indexed
-}
addRefsFrom :: (MonadIO m, NameCacheMonad m) => HieDb -> Maybe FilePath -> SkipOptions -> FilePath ->  m Bool
addRefsFrom c@(getConn -> conn) mSrcBaseDir skipOptions path = do
  hash <- liftIO $ getFileHash path
  mods <- liftIO $ query conn "SELECT * FROM mods WHERE hieFile = ? AND hash = ?" (path, hash)
  case mods of
    (HieModuleRow{}:_) -> pure False
    [] -> do
      withHieFile path $ addRefsWithFile  hash
      pure True
  where
      addRefsWithFile :: MonadIO m => Fingerprint -> HieFile -> m ()
      addRefsWithFile hash hieFile = do
        srcfile <- liftIO $
              maybe
                (pure . FakeFile $ Nothing)
                (\srcBaseDir ->  do
                    srcFullPath <- makeAbsolute (srcBaseDir </> hie_hs_file hieFile)
                    fileExists <- doesFileExist srcFullPath
                    pure $ if fileExists then RealFile srcFullPath else FakeFile Nothing
                )
                mSrcBaseDir
        addRefsFromLoadedInternal c path srcfile hash skipOptions hieFile

addRefsFromLoaded
  :: MonadIO m
  => HieDb -- ^ HieDb into which we're adding the file
  -> FilePath -- ^ Path to @.hie@ file
  -> SourceFile -- ^ Path to .hs file from which @.hie@ file was created
                -- Also tells us if this is a real source file?
                -- i.e. does it come from user's project (as opposed to from project's dependency)?
  -> Fingerprint -- ^ The hash of the @.hie@ file
  -> HieFile -- ^ Data loaded from the @.hie@ file
  -> m ()
addRefsFromLoaded db path sourceFile hash hf =
    addRefsFromLoadedInternal db path sourceFile hash defaultSkipOptions hf

addRefsFromLoadedInternal
  :: MonadIO m
  => HieDb -- ^ HieDb into which we're adding the file
  -> FilePath -- ^ Path to @.hie@ file
  -> SourceFile -- ^ Path to .hs file from which @.hie@ file was created
                -- Also tells us if this is a real source file?
                -- i.e. does it come from user's project (as opposed to from project's dependency)?
  -> Fingerprint -- ^ The hash of the @.hie@ file
  -> SkipOptions -- ^ Skip indexing certain tables
  -> HieFile -- ^ Data loaded from the @.hie@ file
  -> m ()
addRefsFromLoadedInternal
  db@(getConn -> conn) path sourceFile hash skipOptions hf =
    liftIO $ withTransaction conn $ do
      deleteInternalTables conn path
      addRefsFromLoaded_unsafe db path sourceFile hash skipOptions hf

-- | Like 'addRefsFromLoaded' but without:
--   1) using a transaction
--   2) cleaning up previous versions of the file
--
--   Mostly useful to index a new database from scratch as fast as possible
addRefsFromLoaded_unsafe
  :: MonadIO m
  => HieDb -- ^ HieDb into which we're adding the file
  -> FilePath -- ^ Path to @.hie@ file
  -> SourceFile -- ^ Path to .hs file from which @.hie@ file was created
                -- Also tells us if this is a real source file?
                -- i.e. does it come from user's project (as opposed to from project's dependency)?
  -> Fingerprint -- ^ The hash of the @.hie@ file
  -> SkipOptions -- ^ Skip indexing certain tables
  -> HieFile -- ^ Data loaded from the @.hie@ file
  -> m ()
addRefsFromLoaded_unsafe
 db@(getConn -> conn) path sourceFile hash skipOptions hf = liftIO $ do

  let isBoot = "boot" `isSuffixOf` path
      mod    = moduleName smod
      uid    = moduleUnit smod
      smod   = hie_module hf
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
      (srcFile, isReal) = case sourceFile of
        RealFile f -> (Just f, True)
        FakeFile mf -> (mf, False)
      modrow = HieModuleRow path (ModuleInfo mod uid isBoot srcFile isReal hash)

  execute conn "INSERT INTO mods VALUES (?,?,?,?,?,?,?)" modrow

  let AstInfo rows decls imports = genAstInfo path smod refmap

  unless (skipRefs skipOptions) $
    executeMany conn "INSERT INTO refs  VALUES (?,?,?,?,?,?,?,?)" rows
  unless (skipDecls skipOptions) $
    executeMany conn "INSERT INTO decls VALUES (?,?,?,?,?,?,?)" decls
  unless (skipImports skipOptions) $
    executeMany conn "INSERT INTO imports VALUES (?,?,?,?,?,?)" imports

  let defs = genDefRow path smod refmap
  unless (skipDefs skipOptions) $
    forM_ defs $ \def ->
      execute conn "INSERT INTO defs VALUES (?,?,?,?,?,?)" def

  let exports = generateExports path $ hie_exports hf
  unless (skipExports skipOptions) $
    executeMany conn "INSERT INTO exports VALUES (?,?,?,?,?,?,?,?)" exports

  unless (skipTypes skipOptions) $ do
    ixs <- addArr db (hie_types hf)
    unless (skipTypeRefs skipOptions) $ do
      addTypeRefs db path hf ixs

{-| Add path to .hs source given path to @.hie@ file which has already been indexed.
No action is taken if the corresponding @.hie@ file has not been indexed yet.
-}
addSrcFile
  :: HieDb
  -> FilePath -- ^ Path to @.hie@ file
  -> FilePath -- ^ Path to .hs file to be added to DB
  -> Bool -- ^ Is this a real source file? I.e. does it come from user's project (as opposed to from project's dependency)?
  -> IO ()
addSrcFile (getConn -> conn) hie srcFile isReal =
  execute conn "UPDATE mods SET hs_src = ? , is_real = ? WHERE hieFile = ?" (srcFile, isReal, hie)

{-| Remove the path to .hs source for all dependency @.hie@ files. Useful for resetting
the indexed dependencies if the sources have been deleted for some reason.
-}
removeDependencySrcFiles
  :: HieDb
  -> IO ()
removeDependencySrcFiles (getConn -> conn) =
  execute conn "UPDATE mods SET hs_src = NULL WHERE NOT is_real" ()

{-| Delete all occurrences of given @.hie@ file from the database -}
deleteFileFromIndex :: HieDb -> FilePath -> IO ()
deleteFileFromIndex (getConn -> conn) path = withTransaction conn $ do
  deleteInternalTables conn path

{-| Delete all entries associated with modules for which the 'modInfoSrcFile' doesn't exist
on the disk.
Doesn't delete it if there is no associated 'modInfoSrcFile'
-}
deleteMissingRealFiles :: HieDb -> IO ()
deleteMissingRealFiles (getConn -> conn) = withTransaction conn $ do
  missing_file_keys <- fold_ conn "SELECT hieFile,hs_src FROM mods WHERE hs_src IS NOT NULL AND is_real" [] $
    \acc (path,src) -> do
      exists <- doesFileExist src
      pure $ if exists then acc else path : acc
  forM_ missing_file_keys $ \path -> do
    deleteInternalTables conn path

{-| Garbage collect typenames with no references - it is a good idea to call
this function after a sequence of database updates (inserts or deletes)
-}
garbageCollectTypeNames :: HieDb -> IO Int
garbageCollectTypeNames (getConn -> conn) = do
  execute_ conn "DELETE FROM typenames WHERE NOT EXISTS ( SELECT 1 FROM typerefs WHERE typerefs.id = typenames.id LIMIT 1 )"
  changes conn

deleteInternalTables :: Connection -> FilePath -> IO ()
deleteInternalTables conn path = do
  execute conn "DELETE FROM refs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM decls WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM defs  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM typerefs WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM mods  WHERE hieFile = ?" (Only path)
  execute conn "DELETE FROM exports WHERE hieFile = ?" (Only path)

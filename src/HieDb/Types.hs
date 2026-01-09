{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
module HieDb.Types where

import Prelude hiding (mod)

import Data.IORef

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Exception
import Control.Monad (void)
import Control.Monad.Cont (ContT(..))
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.List.NonEmpty (NonEmpty(..))

import Data.Int

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import qualified Text.ParserCombinators.ReadP as R

import HieDb.Compat

data HieDb = HieDb
  { getConn :: !Connection
  , preparedStatements :: HieDbStatements
  }

-- | Record of prepared statements that are relevant during the slowest
-- operation, indexing.
--
-- See benchmarks in https://github.com/wz1000/HieDb/pull/86, where statement
-- preparation results in a ~28% improvement in indexing hie files generated
-- from HLS.
data HieDbStatements = HieDbStatements
  { insertModsStatement :: !(StatementFor HieModuleRow)
  , insertRefsStatement :: !(StatementFor RefRow)
  , insertDeclsStatement :: !(StatementFor DeclRow)
  , insertImportsStatement :: !(StatementFor ImportRow)
  , insertDefsStatement :: !(StatementFor DefRow)
  , insertExportsStatement :: !(StatementFor ExportRow)
  , insertTyperefsStatement :: !(StatementFor TypeRef)
  , insertTypenamesStatement :: !(StatementFor (OccName, ModuleName, Unit))
  , queryTypenamesStatement :: !(StatementFor (OccName, ModuleName, Unit))
  , deleteInternalTablesStatement :: !(Only FilePath -> IO ())
  }

-- | A type-safe wrapper connecting the query preparation with the datatype the
-- query is intended for. The type variable 'a' should correspond to the
-- datatype that will be bound to the statement that is wrapped.
newtype StatementFor a = StatementFor Statement

-- | Equivalent to unit so we can avoid parsing anything when doing insertions.
--
-- Many of our indexing operations are just simple inserts that don't return
-- results. We can't use the array of @sqlite-simple.execute@ functions as they
-- can't make use of the statements we've prepared.
data NoOutput = NoOutput

instance FromRow NoOutput where
  fromRow = pure NoOutput

-- | Helper for preparing multiple statements, each of which has to be
-- bracket-wrapped. Done via ContT, so we can use do/applicative notation.
createStatement :: Connection -> Query -> ContT r IO (StatementFor a)
createStatement connection query = fmap StatementFor (ContT (withStatement connection query))

-- | Run a statement that was built for an datatype in mind, ensuring that we do
-- indeed pass that datatype.
--
-- This function is preferably inlined so it'd get specialized with the datatype
-- encoder.
runStatementFor_ :: ToRow a => StatementFor a -> a -> IO ()
{-# INLINE runStatementFor_ #-}
runStatementFor_ (StatementFor statement) params = do
  withBind statement params $
    -- sqlite-simple doesn't offer the best interface for executing prepared
    -- insertions. `nextRow` requires a hint to know what it needs to parse if it
    -- encounters content from sqlite. Though this code path isn't hit as our
    -- insertions don't return anything, we still need to provide a parsable
    -- datatype. Bit of a leaky abstraction, can be cleaned up by using the
    -- lower-level `direct-sqlite` instead.
    void (nextRow @NoOutput statement)

-- | Run a statement that was built for an datatype in mind, ensuring that we do
-- indeed pass that datatype.
--
-- This function is preferably inlined so it'd get specialized with the datatype
-- encoder.
--
-- NB: While the input variable acts as a witness and is carried around, the
-- output variable is unconstrained. When using this function, double check that
-- the output type is what you expect it to be.
runStatementFor :: (ToRow a, FromRow b) => StatementFor a -> a -> IO (Maybe b)
{-# INLINE runStatementFor #-}
runStatementFor (StatementFor statement) params = do
  withBind statement params $
    nextRow statement

data HieDbException
  = IncompatibleSchemaVersion
  { expectedVersion :: Integer, gotVersion :: Integer }
  deriving (Eq,Ord,Show)

instance Exception HieDbException where

setHieTrace :: HieDb -> Maybe (T.Text -> IO ()) -> IO ()
setHieTrace = setTrace . getConn

-- | Encodes the original haskell source file of a module, along with whether
-- it is "real" or not
-- A file is "real" if it comes from the user project, as opposed to a
-- dependency
data SourceFile = RealFile FilePath | FakeFile (Maybe FilePath)

data ModuleInfo
  = ModuleInfo
  { modInfoName :: ModuleName
  , modInfoUnit :: Unit -- ^ Identifies the package this module is part of
  , modInfoIsBoot :: Bool -- ^ True, when this ModuleInfo was created by indexing @.hie-boot@  file;
                          -- False when it was created from @.hie@ file
  , modInfoSrcFile :: Maybe FilePath -- ^ The path to the haskell source file, from which the @.hie@ file was created
  , modInfoIsReal :: Bool -- ^ Is this a real source file? I.e. does it come from user's project (as opposed to from project's dependency)?
  , modInfoHash :: Fingerprint -- ^ The hash of the @.hie@ file from which this ModuleInfo was created
  } deriving Eq

instance Show ModuleInfo where
  show = show . toRow

instance ToRow ModuleInfo where
  toRow (ModuleInfo a b c d e f) = toRow (a,b,c,d,e,f)
instance FromRow ModuleInfo where
  fromRow = ModuleInfo <$> field <*> field <*> field
                       <*> field <*> field <*> field

type Res a = a :. ModuleInfo

instance ToField ModuleName where
  toField mod = SQLText $ T.pack $ moduleNameString mod
instance FromField ModuleName where
  fromField fld = mkModuleName . T.unpack <$> fromField fld

instance FromRow ModuleName where
  fromRow = field

instance ToField Unit where
  toField uid = SQLText $ T.pack $ unitString uid
instance FromField Unit where
  fromField fld = stringToUnit . T.unpack <$> fromField fld

instance ToField Fingerprint where
  toField hash = SQLText $ T.pack $ show hash
instance FromField Fingerprint where
  fromField fld = readHexFingerprint . T.unpack <$> fromField fld

toNsChar :: NameSpace -> String
toNsChar ns
  | Just fld_par <- fieldNameSpace_maybe ns
  = ('f':unpackFS fld_par) ++ ":"
  | isVarNameSpace ns = "v:"
  | isDataConNameSpace ns = "c:"
  | isTcClsNameSpace ns  = "t:"
  | isTvNameSpace ns = "z:"
  | otherwise = error "namespace not recognized"

fromNsChar :: T.Text -> Maybe NameSpace
fromNsChar ns
  | Just ('f',fieldNameSpace) <- T.uncons ns
  = Just (fieldName $ mkFastStringByteString $ T.encodeUtf8 fieldNameSpace)
fromNsChar "v" = Just varName
fromNsChar "c" = Just dataName
fromNsChar "t" = Just tcClsName
fromNsChar "z" = Just tvName
fromNsChar _   = Nothing

instance ToField OccName where
  toField occ = SQLText $ T.pack $ toNsChar (occNameSpace occ) ++ occNameString occ
instance FromField OccName where
  fromField fld =
    case fieldData fld of
      SQLText t ->
        case T.break (== ':') t of
          (nsText,occ)
            | Just ns <- fromNsChar nsText ->
              return $ mkOccName ns (T.unpack $ T.tail occ)
          _ -> returnError ConversionFailed fld ("OccName encoding invalid: " ++ show t)
      _ -> returnError Incompatible fld "Expected a SQL string representing an OccName"

data HieModuleRow
  = HieModuleRow
  { hieModuleHieFile :: FilePath -- ^ Full path to @.hie@ file based on which this row was created
  , hieModInfo :: ModuleInfo
  } deriving Eq

instance Show HieModuleRow where
  show = show . toRow

instance ToRow HieModuleRow where
  toRow (HieModuleRow a b) =
     toField a : toRow b

instance FromRow HieModuleRow where
  fromRow =
    HieModuleRow <$> field <*> fromRow

data RefRow
  = RefRow
  { refSrc :: FilePath
  , refNameOcc :: OccName
  , refNameMod :: ModuleName
  , refNameUnit :: Unit
  , refSLine :: Int
  , refSCol :: Int
  , refELine :: Int
  , refECol :: Int
  , refIsGenerated :: Bool -- ^ True if the reference to this name is generated by GHC (NodeOrigin is GeneratedInfo)
                           -- False if it comes from the source code (NodeOrigin is SourceInfo)
  }

instance ToRow RefRow where
  toRow (RefRow a b c d e f g h i) = toRow ((a,b,c):.(d,e,f):.(g,h,i))

instance FromRow RefRow where
  fromRow = RefRow <$> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field

data DeclRow
  = DeclRow
  { declSrc :: FilePath
  , declNameOcc :: OccName
  , declSLine :: Int
  , declSCol :: Int
  , declELine :: Int
  , declECol :: Int
  , declRoot :: Bool
  }

instance ToRow DeclRow where
  toRow (DeclRow a b c d e f g) = toRow ((a,b,c,d):.(e,f,g))

instance FromRow DeclRow where
  fromRow = DeclRow <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field

data ImportRow
  = ImportRow
    { importSrc :: FilePath
    , importModuleName :: ModuleName
    , importSLine :: Int
    , importSCol :: Int
    , importELine :: Int
    , importECol :: Int
    }

instance FromRow ImportRow where
  fromRow =
    ImportRow
      <$> field <*> field <*> field <*> field
      <*> field <*> field

instance ToRow ImportRow where
  toRow (ImportRow a b c d e f) = toRow ((a,b,c,d):.(e,f))

data TypeName = TypeName
  { typeName :: OccName
  , typeMod :: ModuleName
  , typeUnit :: Unit
  }

data TypeRef = TypeRef
  { typeRefOccId :: Int64
  , typeRefHieFile :: FilePath
  , typeRefDepth :: Int
  , typeRefSLine :: Int
  , typeRefSCol :: Int
  , typeRefELine :: Int
  , typeRefECol :: Int
  }

instance ToRow TypeRef where
  toRow (TypeRef a b c d e f g) = toRow ((a,b,c,d):.(e,f,g))

instance FromRow TypeRef where
  fromRow = TypeRef <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field

data DefRow
  = DefRow
  { defSrc :: FilePath
  , defNameOcc :: OccName
  , defSLine :: Int
  , defSCol :: Int
  , defELine :: Int
  , defECol :: Int
  }

instance ToRow DefRow where
  toRow (DefRow a b c d e f) = toRow ((a,b,c,d):.(e,f))

instance FromRow DefRow where
  fromRow = DefRow <$> field <*> field <*> field <*> field
                   <*> field <*> field

data ExportRow = ExportRow
  { exportHieFile :: FilePath -- ^ Exporting module
  , exportName :: OccName
  , exportMod :: ModuleName -- ^ Definition module
  , exportUnit :: Unit
  , exportParent :: Maybe OccName
  , exportParentMod :: Maybe ModuleName
  , exportParentUnit :: Maybe Unit
  , exportIsDatacon :: Bool
  }
instance ToRow ExportRow where
  toRow (ExportRow a b c d e f g h) = toRow (a,b,c,d,e,f,g,h)

instance FromRow ExportRow where
  fromRow = ExportRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

{-| Monad with access to 'NameCacheUpdater', which is needed to deserialize @.hie@ files -}
class Monad m => NameCacheMonad m where
  getNcUpdater :: m NameCacheUpdater

newtype DbMonadT m a = DbMonadT { runDbMonad :: ReaderT (IORef NameCache) m a } deriving (MonadTrans)
deriving instance Monad m => Functor (DbMonadT m)
deriving instance Monad m => Applicative (DbMonadT m)
deriving instance Monad m => Monad (DbMonadT m)
deriving instance MonadIO m => MonadIO (DbMonadT m)

type DbMonad = DbMonadT IO

runDbM :: IORef NameCache -> DbMonad a -> IO a
runDbM nc x = flip runReaderT nc $ runDbMonad x

instance MonadIO m => NameCacheMonad (DbMonadT m) where
#if __GLASGOW_HASKELL__ >= 903
  getNcUpdater = DbMonadT $ ReaderT $ \ref -> liftIO $ readIORef ref
#else
  getNcUpdater = DbMonadT $ ReaderT $ \ref -> pure (NCU $ atomicModifyIORef' ref)
#endif


data HieDbErr
  = NotIndexed ModuleName (Maybe Unit)
  | AmbiguousUnitId (NonEmpty ModuleInfo)
  | NameNotFound OccName (Maybe ModuleName) (Maybe Unit)
  | NoNameAtPoint HieTarget (Int,Int)
  | NameUnhelpfulSpan Name String

data Symbol = Symbol
    { symName   :: !OccName
    , symModule :: !Module
    } deriving (Eq, Ord)

instance Show Symbol where
    show s =     toNsChar (occNameSpace $ symName s)
              <> occNameString (symName s)
              <> ":"
              <> moduleNameString (moduleName $ symModule s)
              <> ":"
        --       <> unitIdString (moduleUnit $ symModule s)
              <> unitString (moduleUnit $ symModule s)

instance Read Symbol where
  readsPrec = const $ R.readP_to_S readSymbol

readNameSpace :: R.ReadP NameSpace
readNameSpace = do
  c <- R.many1 R.get
  maybe R.pfail return (fromNsChar $ T.pack c)

readColon :: R.ReadP ()
readColon = () <$ R.char ':'

readSymbol :: R.ReadP Symbol
readSymbol = do
  ns <- readNameSpace
  readColon
  n <- R.many1 R.get
  readColon
  m <- R.many1 R.get
  readColon
  u <- R.many1 R.get
  R.eof
  let mn  = mkModuleName m
      uid = stringToUnit u
      sym = Symbol
              { symName   = mkOccName ns n
              , symModule = mkModule uid mn
              }
  return sym

-- | GHC Library Directory. Typically you'll want to use
-- @libdir@ from <https://hackage.haskell.org/package/ghc-paths ghc-paths>
newtype LibDir = LibDir FilePath

-- | A way to specify which HieFile to operate on.
-- Either the path to @.hie@ file is given in the Left
-- Or ModuleName (with optional Unit) is given in the Right
type HieTarget = Either FilePath (ModuleName, Maybe Unit)

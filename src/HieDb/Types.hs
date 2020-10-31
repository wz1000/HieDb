{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module HieDb.Types where

import Prelude hiding (mod)

import Name
import Module
import NameCache
import DynFlags
import IfaceEnv (NameCacheUpdater(..))
import Data.IORef

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception

import Data.List.NonEmpty (NonEmpty(..))

import Data.Time.Clock
import Data.Int

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import qualified Text.ParserCombinators.ReadP as R

data HieDb = HieDb { getConn :: Connection, getDbDynFlags :: Maybe DynFlags }

data HieDbException
  = IncompatibleSchemaVersion
  { expectedVersion :: Integer, gotVersion :: Integer }
  deriving (Eq,Ord,Show)

instance Exception HieDbException where

setHieTrace :: HieDb -> (Maybe (T.Text -> IO ())) -> IO ()
setHieTrace (getConn -> conn) x = setTrace conn x

data ModuleInfo
  = ModuleInfo
  { modInfoName :: ModuleName
  , modInfoUnit :: UnitId
  , modInfoIsBoot :: Bool
  , modInfoSrcFile :: Maybe FilePath
  , modInfoTime :: UTCTime
  }

instance Show ModuleInfo where
  show = show . toRow

instance ToRow ModuleInfo where
  toRow (ModuleInfo a b c d e) = toRow (a,b,c,d,e)
instance FromRow ModuleInfo where
  fromRow = ModuleInfo <$> field <*> field <*> field <*> field <*> field

type Res a = a :. ModuleInfo

instance ToField ModuleName where
  toField mod = SQLText $ T.pack $ moduleNameString mod
instance FromField ModuleName where
  fromField fld = mkModuleName . T.unpack <$> fromField fld

instance ToField UnitId where
  toField uid = SQLText $ T.pack $ unitIdString uid
instance FromField UnitId where
  fromField fld = stringToUnitId . T.unpack <$> fromField fld

toNsChar :: NameSpace -> Char
toNsChar ns
  | isVarNameSpace ns = 'v'
  | isDataConNameSpace ns = 'c'
  | isTcClsNameSpace ns  = 't'
  | isTvNameSpace ns = 'z'
  | otherwise = error "namespace not recognized"

fromNsChar :: Char -> Maybe NameSpace
fromNsChar 'v' = Just varName
fromNsChar 'c' = Just dataName
fromNsChar 't' = Just tcClsName
fromNsChar 'z' = Just tvName
fromNsChar _ = Nothing

instance ToField OccName where
  toField occ = SQLText $ T.pack $ toNsChar (occNameSpace occ) : occNameString occ
instance FromField OccName where
  fromField fld =
    case fieldData fld of
      SQLText t ->
        case T.uncons t of
          Just (nsChar,occ)
            | Just ns <- fromNsChar nsChar ->
              return $ mkOccName ns (T.unpack occ)
          _ -> returnError ConversionFailed fld "OccName encoding invalid"
      _ -> returnError Incompatible fld "Expected a SQL string representing an OccName"

data HieModuleRow
  = HieModuleRow
  { hieModuleHieFile :: FilePath
  , hieModInfo :: ModuleInfo
  }

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
  , refNameUnit :: UnitId
  , refFile :: FilePath
  , refSLine :: Int
  , refSCol :: Int
  , refELine :: Int
  , refECol :: Int
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
  , declFile :: FilePath
  , declSLine :: Int
  , declSCol :: Int
  , declELine :: Int
  , declECol :: Int
  , declRoot :: Bool
  }

instance ToRow DeclRow where
  toRow (DeclRow a b c d e f g h) = toRow ((a,b,c,d):.(e,f,g,h))

instance FromRow DeclRow where
  fromRow = DeclRow <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field

data TypeName = TypeName
  { typeName :: OccName
  , typeMod :: ModuleName
  , typeUnit :: UnitId
  }

data TypeRef = TypeRef
  { typeRefOccId :: Int64
  , typeRefHieFile :: FilePath
  , typeRefDepth :: Int
  , typeRefFile :: FilePath
  , typeRefSLine :: Int
  , typeRefSCol :: Int
  , typeRefELine :: Int
  , typeRefECol :: Int
  }

instance ToRow TypeRef where
  toRow (TypeRef a b c d e f g h) = toRow ((a,b,c,d):.(e,f,g,h))

instance FromRow TypeRef where
  fromRow = TypeRef <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field

data DefRow
  = DefRow
  { defSrc :: FilePath
  , defNameOcc :: OccName
  , defFile :: FilePath
  , defSLine :: Int
  , defSCol :: Int
  , defELine :: Int
  , defECol :: Int
  , defType :: Maybe String
  , defDoc :: Maybe String
  }

instance ToRow DefRow where
  toRow (DefRow a b c d e f g h i) = toRow ((a,b,c,d):.(e,f,g,h,i))

instance FromRow DefRow where
  fromRow = DefRow <$> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field
                   <*> field


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
  getNcUpdater = DbMonadT $ ReaderT $ \ref -> pure (NCU $ atomicModifyIORef' ref)


data HieDbErr
  = NotIndexed ModuleName (Maybe UnitId)
  | AmbiguousUnitId (NonEmpty ModuleInfo)
  | NameNotFound OccName (Maybe ModuleName) (Maybe UnitId)
  | NameUnhelpfulSpan Name String

data Symbol = Symbol
    { symName   :: !OccName
    , symModule :: !Module
    } deriving (Eq, Ord)

instance Show Symbol where
    show s =  toNsChar (occNameSpace $ symName s)
           :  ':'
           :  occNameString (symName s)
           <> ":"
           <> moduleNameString (moduleName $ symModule s)
           <> ":"
           <> unitIdString (moduleUnitId $ symModule s)

instance Read Symbol where
  readsPrec = const $ R.readP_to_S readSymbol

readNameSpace :: R.ReadP NameSpace
readNameSpace = do
  c <- R.get
  case fromNsChar c of
    Nothing -> R.pfail
    Just ns -> return ns

readColon :: R.ReadP ()
readColon = do
  c <- R.get
  when (c /= ':') R.pfail

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
      uid = stringToUnitId u
      sym = Symbol
              { symName   = mkOccName ns n
              , symModule = mkModule uid mn
              }
  return sym

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module HieDb.Types where

import Prelude hiding (mod)

import Name
import Module
import NameCache

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.List.NonEmpty (NonEmpty(..))

import Data.Time.Clock

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

newtype HieDb = HieDb { getConn :: Connection }

setHieTrace :: HieDb -> (Maybe (T.Text -> IO ())) -> IO ()
setHieTrace (HieDb conn) x = setTrace conn x

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
  , hieModule :: ModuleName
  , hieUnit :: UnitId
  , hieModuleIndexTime :: UTCTime
  }

instance ToRow HieModuleRow where
  toRow (HieModuleRow a b c d) =
     toRow (a, b, c, d)

instance FromRow HieModuleRow where
  fromRow = HieModuleRow <$> field <*> field <*> field <*> field

data RefRow
  = RefRow
  { refSrc :: FilePath
  , refSrcMod :: ModuleName
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
  toRow (RefRow a b c d e f g h i j) = toRow (a,b,c,d,e,f,g,h,i,j)

instance FromRow RefRow where
  fromRow = RefRow <$> field <*> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field <*> field

class Monad m => NameCacheMonad m where
  getNc :: m NameCache
  putNc :: NameCache -> m ()

newtype DbMonadT m a = DbMonadT { runDbMonad :: StateT NameCache m a } deriving (MonadTrans)
deriving instance Monad m => Functor (DbMonadT m)
deriving instance Monad m => Applicative (DbMonadT m)
deriving instance Monad m => Monad (DbMonadT m)
deriving instance MonadIO m => MonadIO (DbMonadT m)

type DbMonad = DbMonadT IO

execDbM :: NameCache -> DbMonad a -> IO a
execDbM nc x = flip evalStateT nc $ runDbMonad x

instance Monad m => NameCacheMonad (DbMonadT m) where
  getNc = DbMonadT get
  putNc = DbMonadT . put

data HieDbErr
  = NotIndexed ModuleName (Maybe UnitId)
  | AmbiguousUnitId (NonEmpty UnitId)

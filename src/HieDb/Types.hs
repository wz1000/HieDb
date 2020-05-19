{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import qualified Text.ParserCombinators.ReadP as R

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
  , refSrcUnit :: UnitId
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
  toRow (RefRow a b c d e f g h i j k) = toRow ((a,b,c,d,e,f):.(g,h,i,j,k))

instance FromRow RefRow where
  fromRow = RefRow <$> field <*> field <*> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field <*> field

data DeclRow
  = DeclRow
  { declSrc :: FilePath
  , declMod :: ModuleName
  , declUnit :: UnitId
  , declNameOcc :: OccName
  , declFile :: FilePath
  , declSLine :: Int
  , declSCol :: Int
  , declELine :: Int
  , declECol :: Int
  , declRoot :: Bool
  }

instance ToRow DeclRow where
  toRow (DeclRow a b c d e f g h i j) = toRow ((a,b,c,d,e):.(f,g,h,i,j))

instance FromRow DeclRow where
  fromRow = DeclRow <$> field <*> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field <*> field

data DefRow
  = DefRow
  { defSrc :: FilePath
  , defMod :: ModuleName
  , defUnit :: UnitId
  , defNameOcc :: OccName
  , defFile :: FilePath
  , defSLine :: Int
  , defSCol :: Int
  , defELine :: Int
  , defECol :: Int
  }

instance ToRow DefRow where
  toRow (DefRow a b c d e f g h i) = toRow ((a,b,c,d,e,f):.(g,h,i))

instance FromRow DefRow where
  fromRow = DefRow <$> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field


class Monad m => NameCacheMonad m where
  getNc :: m NameCache
  putNc :: NameCache -> m ()

newtype DbMonadT m a = DbMonadT { runDbMonad :: StateT NameCache m a } deriving (MonadTrans)
deriving instance Monad m => Functor (DbMonadT m)
deriving instance Monad m => Applicative (DbMonadT m)
deriving instance Monad m => Monad (DbMonadT m)
deriving instance MonadIO m => MonadIO (DbMonadT m)

type DbMonad = DbMonadT IO

evalDbM :: NameCache -> DbMonad a -> IO a
evalDbM nc x = flip evalStateT nc $ runDbMonad x

execDbM :: NameCache -> DbMonad a -> IO NameCache
execDbM nc x = flip execStateT nc $ runDbMonad x

runDbM :: NameCache -> DbMonad a -> IO (a,NameCache)
runDbM nc x = flip runStateT nc $ runDbMonad x

instance Monad m => NameCacheMonad (DbMonadT m) where
  getNc = DbMonadT get
  putNc = DbMonadT . put

data HieDbErr
  = NotIndexed ModuleName (Maybe UnitId)
  | AmbiguousUnitId (NonEmpty UnitId)
  | NameNotFound OccName ModuleName (Maybe UnitId)
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

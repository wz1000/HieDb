{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where
  
import GHC
import HieBin
import HieTypes
import HieUtils
import Name
import Module
import NameCache
import OccName
import UniqSupply


import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import qualified FastString as FS

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Mem
import System.Exit

import System.Console.Terminal.Size

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy hiding (First)
import Control.Monad.Reader

import Data.Char
import Data.Maybe
import Data.List (intercalate)
import Data.Coerce
import Data.Semigroup

import qualified Data.ByteString as BS

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import Data.Time.Clock

import Options.Applicative

instance ToField ModuleName where
  toField mod = SQLText $ T.pack $ moduleNameString mod
instance FromField ModuleName where
  fromField field = mkModuleName . T.unpack <$> fromField field

instance ToField UnitId where
  toField uid = SQLText $ T.pack $ unitIdString uid
instance FromField UnitId where
  fromField field = stringToUnitId . T.unpack <$> fromField field

toNsChar :: NameSpace -> Char
toNsChar ns
  | isVarNameSpace ns = 'v'
  | isDataConNameSpace ns = 'c'
  | isTcClsNameSpace ns  = 't'
  | isTvNameSpace ns = 'z'

fromNsChar :: Char -> Maybe NameSpace
fromNsChar 'v' = Just varName
fromNsChar 'c' = Just dataName
fromNsChar 't' = Just tcClsName
fromNsChar 'z' = Just tvName
fromNsChar _ = Nothing

instance ToField OccName where
  toField occ = SQLText $ T.pack $ toNsChar (occNameSpace occ) : occNameString occ
instance FromField OccName where
  fromField field =
    case fieldData field of
      SQLText t ->
        case T.uncons t of
          Just (nsChar,occ)
            | Just ns <- fromNsChar nsChar ->
              return $ mkOccName ns (T.unpack occ)
          _ -> returnError ConversionFailed field "OccName encoding invalid"
      _ -> returnError Incompatible field "Expected a SQL string representing an OccName"

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

type NcMonadT m a = StateT DbState m a

data DbState
  = DbState
  { total :: Int
  , done :: Int
  , namecache :: NameCache
  }

data Env = Env { getConn :: Connection
               , getOptions :: Options
               }
type DbMonad a = NcMonadT (ReaderT Env IO) a

generateRefs
  :: Foldable f
  => f (HieAST a)
  -> [(Identifier,Span)]
generateRefs = concatMap go
  where
    go ast = this ++ concatMap go (nodeChildren ast)
      where
        this = (,nodeSpan ast) <$> M.keys (nodeIdentifiers $ nodeInfo ast)

genRefRow :: FilePath -> HieFile -> [RefRow]
genRefRow path hf = genRows $ generateRefs $ getAsts $ hie_asts hf
  where
    genRows = mapMaybe go
    go ((Right name, sp))
      | Just mod <- nameModule_maybe name = Just $
          RefRow path smod occ (moduleName mod) (moduleUnitId mod) file sl sc el ec
          where
            smod = moduleName $ hie_module hf
            occ = nameOccName name
            file = FS.unpackFS $ srcSpanFile sp
            sl = srcSpanStartLine sp
            sc = srcSpanStartCol sp
            el = srcSpanEndLine sp
            ec = srcSpanEndCol sp
    go _ = Nothing

addRefsFrom :: FilePath -> DbMonad ()
addRefsFrom path = do
  conn <- asks getConn
  time <- liftIO $ getModificationTime path
  mods <- liftIO $ query conn "SELECT * FROM mods WHERE hieFile = ? AND time >= ?" (path, time)
  case mods of
    (HieModuleRow{}:_) -> return ()
    [] -> withHieFile path $ \hf -> liftIO $ withTransaction conn $ do
      execute conn "DELETE FROM refs WHERE src = ?" (Only path)
      let mod = moduleName $ hie_module hf
          uid = moduleUnitId $ hie_module hf
          modrow = HieModuleRow path mod uid time
      execute conn "INSERT INTO mods VALUES (?,?,?,?)" modrow
      let rows = genRefRow path hf
      executeMany conn "INSERT INTO refs VALUES (?,?,?,?,?,?,?,?,?,?)" rows

withHieFile :: (MonadState DbState m, MonadIO m)
            => FilePath
            -> (HieFile -> m ())
            -> m ()
withHieFile path act = do
  nc <- gets namecache
  (hiefile, nc') <- liftIO $ readHieFile nc path
  modify' (\s -> s {namecache = nc'})
  act (hie_file_result hiefile)

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

execDbM :: Env -> DbState -> DbMonad a -> IO a
execDbM env st x = flip runReaderT env $ flip evalStateT st x

-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <- doesPathExist path
  if exists then do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile && ("hie" `isExtensionOf` path) then do
      path' <- canonicalizePath path
      return [path']
    else if isDir then do
      cnts <- listDirectory path
      withCurrentDirectory path $ foldMap getHieFilesIn cnts
    else return []
  else
    return []

data HieDbConf =
  HieDbConf
  { in_dir :: FilePath
  , ofile :: FilePath
  }

initConn :: Connection -> IO ()
initConn conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS refs (src TEXT, srcMod TEXT, occ TEXT, mod TEXT, unit TEXT, file TEXT, sl INTEGER, sc INTEGER, el INTEGER, ec INTEGER)"
  execute_ conn "CREATE TABLE IF NOT EXISTS mods (hieFile TEXT PRIMARY KEY ON CONFLICT REPLACE, mod TEXT, unit TEXT, time TEXT, CONSTRAINT modid UNIQUE (mod, unit))"

main :: IO ()
main = do
  args <- getArgs
  defdb <- fromMaybe "~/.hiedb" <$> lookupEnv "HIEDB"
  hSetBuffering stdout NoBuffering
  (opts, cmd) <- execParser $ progParseInfo defdb
  runCommand opts cmd


{- USAGE
Some default db location overridden by environment var HIEDB
hiedb init <foo.hiedb>
hiedb index [<dir>...] [hiedb]
hiedb name-refs <name> <module> [unitid] [hiedb]
hiedb type-refs <name> <module> [unitid] [hiedb]
hiedb query-pos <file.hie> <row> <col> [hiedb]
hiedb query-pos --hiedir=<dir> <file.hs> <row> <col> [hiedb]
hiedb cat <module> [unitid]
-}

data Options
  = Options
  { database :: FilePath
  , trace :: Bool
  , quiet :: Bool
  , virtualFile :: Bool
  }

data Command
  = Init
  | Index [FilePath]
  | NameRefs String (Maybe ModuleName) (Maybe UnitId)
  | TypeRefs String (Maybe ModuleName) (Maybe UnitId)
  | Cat ModuleName (Maybe UnitId)

progParseInfo :: FilePath -> ParserInfo (Options, Command)
progParseInfo db = info (progParser db <**> helper)
  ( fullDesc
  <> progDesc "Query .hie files"
  <> header "hiedb - a tool to query groups of .hie files" )

progParser :: FilePath -> Parser (Options,Command)
progParser db = (,) <$> optParser db <*> cmdParser

optParser :: FilePath -> Parser Options
optParser defdb
    = Options
  <$> strOption (long "database" <> short 'D' <> metavar "DATABASE"
              <> value defdb <> showDefault <> help "References Database")
  <*> switch (long "trace" <> short 'v')
  <*> switch (long "quiet" <> short 'q')
  <*> switch (long "virtual-file" <> short 'f')

cmdParser :: Parser Command
cmdParser
   = hsubparser
   $ command "init" (info (pure Init) $ progDesc "Initialize databse")
  <> command "index" (info (Index <$> many (strArgument (metavar "DIRECTORY..."))) $ progDesc "Index database")
  <> command "name-refs" (info (NameRefs <$> (strArgument (metavar "NAME"))
                                         <*> optional (mkModuleName <$> strArgument (metavar "MODULE"))
                                         <*> optional (stringToUnitId <$> strArgument (metavar "UNITID")))
                          $ progDesc "Lookup references of value MODULE.NAME")
  <> command "type-refs" (info (TypeRefs <$> (strArgument (metavar "NAME"))
                                         <*> optional (mkModuleName <$> strArgument (metavar "MODULE"))
                                         <*> optional (stringToUnitId <$> strArgument (metavar "UNITID")))
                          $ progDesc "Lookup references of type MODULE.NAME")
  <> command "cat" (info (Cat <$> (mkModuleName <$> strArgument (metavar "MODULE"))
                              <*> optional (stringToUnitId <$> strArgument (metavar "UNITID")))
                          $ progDesc "Dump contents of MODULE as stored in the hiefile")

progress :: Int -> (FilePath -> DbMonad a) -> FilePath -> DbMonad a
progress l act f = do
  t <- gets total
  cur <- gets done
  liftIO $ putStr $ replicate l ' '
  liftIO $ putStr "\r"
  let msg = (take (l-8) $ unwords ["Processing file", show (cur + 1) ++ "/" ++ show t ++ ":", f]) ++ "..."
  liftIO $ putStr msg
  x <- act f
  liftIO $ putStr " done\r"
  modify' (\s -> s {done = cur + 1})
  return x

resolveUnitId :: Connection -> ModuleName -> IO UnitId
resolveUnitId conn mn = do
  luid <- query conn "SELECT unit FROM mods WHERE mod = ?" (Only mn)
  case (luid :: [Only UnitId]) of
    [] -> do
      putStrLn $ unwords ["Module", moduleNameString mn, "isn't indexed"]
      exitFailure
    [x] -> return $ fromOnly x
    xs -> do
      putStrLn $ "Please specify the unitid, it could be any of: " ++ intercalate ", " (map (show . fromOnly) xs)
      exitFailure

reportRefs :: [RefRow] -> IO ()
reportRefs xs = forM_ xs $ \x -> do
  putStr (moduleNameString $ refSrcMod x)
  putStr ":"
  putStr (show $ refSLine x)
  putStr ":"
  putStr (show $ refSCol x)
  putStr "-"
  putStr (show $ refELine x)
  putStr ":"
  putStrLn (show $ refECol x)

isCons :: String -> Bool
isCons (':':xs) = True
isCons (x:xs) | isUpper x = True
isCons _ = False

search :: Connection -> OccName -> Maybe ModuleName -> Maybe UnitId -> IO [RefRow]
search conn occ (Just mn) Nothing =
  query conn "SELECT * FROM refs WHERE occ = ? AND mod = ?" (occ, mn)
search conn occ (Just mn) (Just uid) =
  query conn "SELECT * FROM refs WHERE occ = ? AND mod = ? AND unit = ?" (occ, mn, uid)
search conn occ _ _=
  query conn "SELECT * FROM refs WHERE occ = ?" (Only occ)

runCommand :: Options -> Command -> IO ()
runCommand opts c = withConnection (database opts) $ \conn -> do
  when (trace opts) $
    setTrace conn (Just $ T.hPutStrLn stderr . ("\n****TRACE: "<>))
  go conn c
  where
    go conn Init = initConn conn
    go conn (Index dirs) = do
      initConn conn
      files <- concat <$> mapM getHieFilesIn dirs
      nc <- makeNc
      wsize <- maybe 80 width <$> size
      execDbM (Env conn opts) (DbState (length files) 0 nc) $
        mapM_ (progress wsize addRefsFrom) files
      putStrLn "\nCompleted!"
    go conn (TypeRefs typ mn muid) = do
      let occ = mkOccName tcClsName typ
      refs <- search conn occ mn muid
      reportRefs refs
    go conn (NameRefs nm mn muid) = do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      refs <- search conn occ mn muid
      reportRefs refs
    go conn (Cat mn muid) = do
      uid <- maybe (resolveUnitId conn mn) return muid
      files <- query conn "SELECT * FROM mods WHERE mod = ? AND unit = ?" (mn, uid)
      case files of
        [] -> do
          putStrLn "No such module indexed"
          exitFailure
        [x] -> do
          nc <- makeNc
          flip evalStateT (DbState 0 0 nc) $ withHieFile (hieModuleHieFile x) (liftIO . BS.putStrLn . hie_hs_src)
        xs -> do
          putStrLn $ "Please specify the unitid, it could be any of: " ++ intercalate ", " (map (show . hieUnit) xs)
          exitFailure

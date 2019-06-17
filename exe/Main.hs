{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (mod)

import GHC
import HieTypes
import HieUtils
import Name
import Module

import qualified Data.Map as M

import qualified Data.Text.IO as T


import System.Environment
import System.Directory
import System.IO
import System.Exit

import System.Console.Terminal.Size

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Either
import Data.List (intercalate)
import Data.Foldable

import qualified Data.ByteString.Char8 as BS

import Options.Applicative

import HieDb

main :: IO ()
main = do
  defaultLoc <- getXdgDirectory XdgData "default.hiedb"
  defdb <- fromMaybe defaultLoc <$> lookupEnv "HIEDB"
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
  | Cat HieTarget
  | RefsAtPoint HieTarget (Int,Int) (Maybe (Int,Int))
  | TypesAtPoint HieTarget (Int,Int) (Maybe (Int,Int))

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
  <> command "cat" (info (Cat <$> hieTarget)
                         $ progDesc "Dump contents of MODULE as stored in the hiefile")
  <> command "point-refs"
        (info (RefsAtPoint <$> hieTarget
                           <*> ((,) <$> argument auto (metavar "SLINE") <*> argument auto (metavar "SCOL"))
                           <*> optional ((,) <$> argument auto (metavar "ELINE") <*> argument auto (metavar "ECOL")))
              $ progDesc "Find references for symbol at point/span")
  <> command "point-types"
        (info (TypesAtPoint <$> hieTarget
                            <*> ((,) <$> argument auto (metavar "SLINE") <*> argument auto (metavar "SCOL"))
                            <*> optional ((,) <$> argument auto (metavar "ELINE") <*> argument auto (metavar "ECOL")))
              $ progDesc "List types of ast at point/span")

type HieTarget = Either FilePath (ModuleName,Maybe UnitId)

maybeUnitId :: Parser (Maybe UnitId)
maybeUnitId =
  optional (stringToUnitId <$> strOption (short 'u' <> long "unit-id" <> metavar "UNITID"))

hieTarget :: Parser HieTarget
hieTarget =
      (Left <$> strOption (long "hiefile" <> short 'f' <> metavar "HIEFILE"))
  <|> (Right <$> ((,) <$> (mkModuleName <$> strArgument (metavar "MODULE")) <*> maybeUnitId))

progress :: Int -> Int -> Int -> (FilePath -> DbMonad a) -> FilePath -> DbMonad a
progress l total cur act f = do
  liftIO $ putStr $ replicate l ' '
  liftIO $ putStr "\r"
  let msg = (take (l-8) $ unwords ["Processing file", show (cur + 1) ++ "/" ++ show total ++ ":", f]) ++ "..."
  liftIO $ putStr msg
  x <- act f
  liftIO $ putStr " done\r"
  return x

runCommand :: Options -> Command -> IO ()
runCommand opts c = withHieDb (database opts) $ \conn -> do
  when (trace opts) $
    setHieTrace conn (Just $ T.hPutStrLn stderr . ("\n****TRACE: "<>))
  go conn c
  where
    go conn Init = initConn conn
    go conn (Index dirs) = do
      initConn conn
      files <- concat <$> mapM getHieFilesIn dirs
      nc <- makeNc
      wsize <- maybe 80 width <$> size
      let progress' = if quiet opts then (\_ _ _ k -> k) else progress
      execDbM nc $ sequence_ $
        zipWith (\f n -> progress' wsize (length files) n (addRefsFrom conn) f) files [0..]
      unless (quiet opts) $
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
    go conn (Cat target) = hieFileCommand conn target (BS.putStrLn . hie_hs_src)
    go conn (RefsAtPoint target sp mep) = hieFileCommand conn target $ \hf -> do
      let names = concat $ pointCommand hf sp mep $ rights . M.keys . nodeIdentifiers . nodeInfo
      forM_ names $ \name -> do
        putStrLn $ unwords ["Name", occNameString (nameOccName name),"at",show sp,"is used in:"]
        case nameModule_maybe name of
          Just mod -> do
            reportRefs =<< search conn (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod)
          Nothing -> do
            let refmap = generateReferencesMap (getAsts $ hie_asts hf)
                refs = map (toRef . fst) $ fromMaybe [] $ M.lookup (Right name) refmap
                toRef spn = (moduleName $ hie_module hf,
                              (srcSpanStartLine spn , srcSpanStartCol spn),
                              (srcSpanEndLine spn , srcSpanEndCol spn))
            reportRefSpans refs
    go conn (TypesAtPoint target sp mep) = hieFileCommand conn target $ \hf -> do
      let types' = concat $ pointCommand hf sp mep $ nodeType . nodeInfo
          types = map (flip recoverFullType $ hie_types hf) types'
      dynFlags <- dynFlagsForPrinting
      forM_ types $ \typ -> do
        putStrLn $ renderHieType dynFlags typ

hieFileCommand :: HieDb -> HieTarget -> (HieFile -> IO a) -> IO a
hieFileCommand conn target f = join $ reportAmbiguousErr =<< withTarget conn target f

reportAmbiguousErr :: Either HieDbErr a -> IO a
reportAmbiguousErr (Right x) = return x
reportAmbiguousErr (Left (NotIndexed mn muid)) = do
  putStrLn $ unwords $ ["Module",moduleNameString mn ++ maybe "" (\uid -> "("++show uid++")") muid, "not indexed."]
  exitFailure
reportAmbiguousErr (Left (AmbiguousUnitId xs)) = do
  putStrLn $ unwords $ ["UnitId could be any of:",intercalate "," (map show $ toList xs)]
  exitFailure

reportRefSpans :: [(ModuleName,(Int,Int),(Int,Int))] -> IO ()
reportRefSpans xs = forM_ xs $ \(mn,(sl,sc),(el,ec)) -> do
  putStr (moduleNameString mn)
  putStr ":"
  putStr (show sl)
  putStr ":"
  putStr (show sc)
  putStr "-"
  putStr (show el)
  putStr ":"
  putStrLn (show ec)

reportRefs :: [RefRow] -> IO ()
reportRefs xs = reportRefSpans
  [ (refSrcMod x,(refSLine x, refSCol x),(refELine x, refECol x))
  | x <- xs
  ]

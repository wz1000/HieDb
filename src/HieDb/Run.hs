{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module HieDb.Run where

import Prelude hiding (mod)

import GHC
import Compat.HieTypes
import Compat.HieUtils
import Name
import Module
import Outputable ((<+>),hang,showSDoc,ppr,text)

import qualified FastString as FS

import qualified Data.Map as M

import qualified Data.Text.IO as T


import System.Environment
import System.Directory
import System.IO
import System.Exit
import System.Time.Extra

import System.Console.Terminal.Size

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Either
import Data.Foldable
import Data.IORef

import qualified Data.ByteString.Char8 as BS

import Options.Applicative

import HieDb
import HieDb.Dump

hiedbMain :: LibDir -> IO ()
hiedbMain libdir = do
  defaultLoc <- getXdgDirectory XdgData $ "default_"++ show dB_VERSION ++".hiedb"
  defdb <- fromMaybe defaultLoc <$> lookupEnv "HIEDB"
  hSetBuffering stdout NoBuffering
  (opts, cmd) <- execParser $ progParseInfo defdb
  runCommand libdir opts cmd


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
  | NameDef  String (Maybe ModuleName) (Maybe UnitId)
  | TypeDef  String (Maybe ModuleName) (Maybe UnitId)
  | Cat HieTarget
  | Ls
  | Rm [HieTarget]
  | ModuleUIDs ModuleName
  | LookupHieFile ModuleName (Maybe UnitId)
  | RefsAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | TypesAtPoint HieTarget (Int,Int) (Maybe (Int,Int))
  | DefsAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | InfoAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | RefGraph
  | Dump FilePath
  | Reachable [Symbol]
  | Unreachable [Symbol]
  | Html [Symbol]

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
  <*> switch (long "trace" <> short 'v' <> help "Print SQL queries being executed")
  <*> switch (long "quiet" <> short 'q' <> help "Don't print progress messages")
  <*> switch (long "virtual-file" <> short 'f' <> internal)

cmdParser :: Parser Command
cmdParser
   = hsubparser
   $ command "init" (info (pure Init) $ progDesc "Initialize database")
  <> command "index" (info (Index <$> many (strArgument (metavar "DIRECTORY..."))) $ progDesc "Index database")
  <> command "name-refs" (info (NameRefs <$> strArgument (metavar "NAME")
                                         <*> optional (mkModuleName <$> strArgument (metavar "MODULE"))
                                         <*> maybeUnitId)
                         $ progDesc "Lookup references of value MODULE.NAME")
  <> command "type-refs" (info (TypeRefs <$> strArgument (metavar "NAME")
                                         <*> optional moduleNameParser
                                         <*> maybeUnitId)
                         $ progDesc "Lookup references of type MODULE.NAME")
  <> command "name-def" (info (NameDef <$> strArgument (metavar "NAME")
                                       <*> optional moduleNameParser
                                       <*> maybeUnitId)
                         $ progDesc "Lookup definition of value MODULE.NAME")
  <> command "type-def" (info (TypeDef <$> strArgument (metavar "NAME")
                                       <*> optional moduleNameParser
                                       <*> maybeUnitId)
                         $ progDesc "Lookup definition of type MODULE.NAME")
  <> command "cat" (info (Cat <$> hieTarget)
                         $ progDesc "Dump contents of MODULE as stored in the hiefile")
  <> command "ls" (info (pure Ls)
                         $ progDesc "List all indexed files/modules")
  <> command "rm" (info (Rm <$> many hieTarget)
                         $ progDesc "Remove targets from index")
  <> command "module-uids" (info (ModuleUIDs <$> moduleNameParser)
                         $ progDesc "List all the UnitIds MODULE is indexed under in the db")
  <> command "lookup-hie" (info (LookupHieFile <$> moduleNameParser <*> maybeUnitId)
                         $ progDesc "Lookup the location of the .hie file corresponding to MODULE")
  <> command "point-refs"
        (info (RefsAtPoint <$> hieTarget
                           <*> posParser 'S'
                           <*> optional (posParser 'E'))
              $ progDesc "Find references for symbol at point/span")
  <> command "point-types"
        (info (TypesAtPoint <$> hieTarget
                            <*> posParser 'S'
                            <*> optional (posParser 'E'))
              $ progDesc "List types of ast at point/span")
  <> command "point-defs"
        (info (DefsAtPoint <$> hieTarget
                            <*> posParser 'S'
                            <*> optional (posParser 'E'))
              $ progDesc "Find definition for symbol at point/span")
  <> command "point-info"
        (info (InfoAtPoint <$> hieTarget
                            <*> posParser 'S'
                            <*> optional (posParser 'E'))
              $ progDesc "Print name, module name, unit id for symbol at point/span")
  <> command "ref-graph" (info (pure RefGraph) $ progDesc "Generate a reachability graph")
  <> command "dump" (info (Dump <$> strArgument (metavar "HIE")) $ progDesc "Dump a HIE AST")
  <> command "reachable" (info (Reachable <$> some symbolParser)
                         $ progDesc "Find all symbols reachable from the given symbols")
  <> command "unreachable" (info (Unreachable <$> some symbolParser)
                           $ progDesc "Find all symbols unreachable from the given symbols")
  <> command "html" (info (Html <$> some symbolParser)
                    $ progDesc "generate html files for reachability from the given symbols")

posParser :: Char -> Parser (Int,Int)
posParser c = (,) <$> argument auto (metavar $ c:"LINE") <*> argument auto (metavar $ c:"COL")

maybeUnitId :: Parser (Maybe UnitId)
maybeUnitId =
  optional (stringToUnitId <$> strOption (short 'u' <> long "unit-id" <> metavar "UNITID"))

symbolParser :: Parser Symbol
symbolParser = argument auto $ metavar "SYMBOL"

moduleNameParser :: Parser ModuleName
moduleNameParser = mkModuleName <$> strArgument (metavar "MODULE")

hieTarget :: Parser HieTarget
hieTarget =
      (Left <$> strOption (long "hiefile" <> short 'f' <> metavar "HIEFILE"))
  <|> (Right <$> ((,) <$> moduleNameParser  <*> maybeUnitId))

progress :: Maybe Int -> Int -> Int -> (FilePath -> DbMonad a) -> FilePath -> DbMonad a
progress mw total cur act f = do
  let msg' = unwords ["Processing file", show (cur + 1) ++ "/" ++ show total ++ ":", f] ++ "..."
  msg <- liftIO $ case mw of
    Nothing -> putStrLn "" >> pure msg'
    Just w -> do
      putStr $ replicate w ' '
      putStr "\r"
      pure $ take (w-8) $ msg'
  liftIO $ putStr msg
  x <- act f
  liftIO $ putStr " done\r"
  return x

runCommand :: LibDir -> Options -> Command -> IO ()
runCommand libdir opts cmd = withHieDbAndFlags libdir (database opts) $ \dynFlags conn -> do
  when (trace opts) $
    setHieTrace conn (Just $ T.hPutStrLn stderr . ("\n****TRACE: "<>))
  case cmd of
    Init -> initConn conn
    Index dirs -> do
      initConn conn
      files <- concat <$> mapM getHieFilesIn dirs
      nc <- newIORef =<< makeNc
      wsize <- fmap width <$> size
      let progress' = if quiet opts then (\_ _ _ k -> k) else progress
      start <- offsetTime
      runDbM nc $
        zipWithM_ (\f n -> progress' wsize (length files) n (addRefsFrom conn) f) files [0..]
      end <- start
      unless (quiet opts) $
        putStrLn $ "\nCompleted! (" <> showDuration end <> ")"
    TypeRefs typ mn muid -> do
      let occ = mkOccName tcClsName typ
      refs <- findReferences conn False occ mn muid []
      reportRefs refs
    NameRefs nm mn muid -> do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      refs <- findReferences conn False occ mn muid []
      reportRefs refs
    NameDef nm mn muid -> do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      (row:.inf) <- reportAmbiguousErr =<< findOneDef conn occ mn muid
      let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
      reportRefSpans [(mdl, (defSLine row, defSCol row), (defELine row, defECol row))]
    TypeDef nm mn muid -> do
      let occ = mkOccName tcClsName nm
      (row:.inf) <- reportAmbiguousErr =<< findOneDef conn occ mn muid
      let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
      reportRefSpans [(mdl, (defSLine row, defSCol row), (defELine row, defECol row))]
    Cat target -> hieFileCommand conn target (BS.putStrLn . hie_hs_src)
    Ls -> do
      mods <- getAllIndexedMods conn
      forM_ mods $ \mod -> do
        putStr $ hieModuleHieFile mod
        putStr "\t"
        putStr $ moduleNameString $ modInfoName $ hieModInfo mod
        putStr "\t"
        putStrLn $ unitIdString $ modInfoUnit $ hieModInfo mod
    Rm targets -> do
        forM_ targets $ \target -> do
          case target of
            Left f -> do
              dir <- doesDirectoryExist f
              if dir
              then do
                fs <- getHieFilesIn f
                mapM_ (deleteFileFromIndex conn) fs
              else do
                cf <- canonicalizePath f
                deleteFileFromIndex conn cf
            Right (mn,muid) -> do
              uid <- reportAmbiguousErr =<< maybe (resolveUnitId conn mn) (return . Right) muid
              mFile <- lookupHieFile conn mn uid
              case mFile of
                Nothing -> reportAmbiguousErr $ Left (NotIndexed mn $ Just uid)
                Just x -> deleteFileFromIndex conn (hieModuleHieFile x)
    ModuleUIDs mn ->
      print =<< reportAmbiguousErr =<< resolveUnitId conn mn
    LookupHieFile mn muid -> reportAmbiguousErr =<< do
      euid <- maybe (resolveUnitId conn mn) (return . Right) muid
      case euid of
        Left err -> return $ Left err
        Right uid -> do
          mFile <- lookupHieFile conn mn uid
          case mFile of
            Nothing -> return $ Left (NotIndexed mn $ Just uid)
            Just x -> Right <$> putStrLn (hieModuleHieFile x)
    RefsAtPoint target sp mep -> hieFileCommand conn target $ \hf -> do
      let names = concat $ pointCommand hf sp mep $ rights . M.keys . nodeIdentifiers . nodeInfo
      forM_ names $ \name -> do
        putStrLn $ unwords ["Name", occNameString (nameOccName name),"at",show sp,"is used in:"]
        case nameModule_maybe name of
          Just mod -> do
            reportRefs =<< findReferences conn False (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) []
          Nothing -> do
            let refmap = generateReferencesMap (getAsts $ hie_asts hf)
                refs = map (toRef . fst) $ M.findWithDefault [] (Right name) refmap
                toRef spn = (hie_module hf,
                              (srcSpanStartLine spn , srcSpanStartCol spn),
                              (srcSpanEndLine spn , srcSpanEndCol spn))
            reportRefSpans refs
    TypesAtPoint target sp mep -> hieFileCommand conn target $ \hf -> do
      let types' = concat $ pointCommand hf sp mep $ nodeType . nodeInfo
          types = map (flip recoverFullType $ hie_types hf) types'
      forM_ types $ \typ -> do
        putStrLn $ renderHieType dynFlags typ
    DefsAtPoint target sp mep -> hieFileCommand conn target $ \hf -> do
      let names = concat $ pointCommand hf sp mep $ rights . M.keys . nodeIdentifiers . nodeInfo
      forM_ names $ \name -> do
        case nameSrcSpan name of
          RealSrcSpan dsp -> do
            putStrLn $ unwords ["Name", occNameString (nameOccName name),"at",show sp,"is defined at:"]
            reportRefSpans [(fromMaybe (hie_module hf) (nameModule_maybe name)
                            ,(srcSpanStartLine dsp,srcSpanStartCol dsp)
                            ,(srcSpanEndLine dsp, srcSpanEndCol dsp))]
          UnhelpfulSpan msg -> do
            case nameModule_maybe name of
              Just mod -> do
                (row:.inf) <- reportAmbiguousErr
                    =<< findOneDef conn (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod)
                putStrLn $ unwords ["Name", occNameString (nameOccName name),"at",show sp,"is defined at:"]
                reportRefSpans [(mkModule (modInfoUnit inf) (modInfoName inf)
                                ,(defSLine row,defSCol row)
                                ,(defELine row,defECol row))]
              Nothing -> do
                reportAmbiguousErr $ Left $ NameUnhelpfulSpan name (FS.unpackFS msg)
    InfoAtPoint target sp mep -> hieFileCommand conn target $ \hf -> do
      mapM_ (uncurry $ printInfo dynFlags) $ pointCommand hf sp mep $ \ast ->
        (renderHieType dynFlags . flip recoverFullType (hie_types hf) <$> nodeInfo ast, nodeSpan ast)
    RefGraph -> declRefs conn
    Dump path -> do
      nc <- newIORef =<< makeNc
      runDbM nc $ dump dynFlags path
    Reachable s -> getReachable conn s >>= mapM_ print
    Unreachable s -> getUnreachable conn s >>= mapM_ print
    Html s -> do
      nc <- newIORef =<< makeNc
      runDbM nc $ html conn s

printInfo :: DynFlags -> NodeInfo String -> RealSrcSpan -> IO ()
printInfo dynFlags x sp = do
  putStrLn $ "Span: " ++ showSDoc dynFlags (ppr sp)
  putStrLn $ "Constructors: " ++ showSDoc dynFlags (ppr $ nodeAnnotations x)
  putStrLn "Identifiers:"
  let idents = M.toList $ nodeIdentifiers x
  forM_ idents $ \(ident,inf) -> do
    case ident of
      Left mdl -> putStrLn $ "Module: " ++ moduleNameString mdl
      Right nm -> do
        case nameModule_maybe nm of
          Nothing -> pure ()
          Just m -> do
            putStr "Symbol:"
            print $ Symbol (nameOccName nm) m
        putStrLn $ showSDoc dynFlags $
          hang (ppr nm <+> text "defined at" <+> ppr (nameSrcSpan nm)) 4 (ppr inf)
  putStrLn "Types:"
  let types = nodeType x
  forM_ types $ \typ -> do
    putStrLn typ
  putStrLn ""

hieFileCommand :: HieDb -> HieTarget -> (HieFile -> IO a) -> IO a
hieFileCommand conn target f = join $ reportAmbiguousErr =<< withTarget conn target f

reportAmbiguousErr :: Either HieDbErr a -> IO a
reportAmbiguousErr (Right x) = return x
reportAmbiguousErr (Left e) = do
  putStrLn $ showHieDbErr e
  exitFailure

showHieDbErr :: HieDbErr -> String
showHieDbErr e = case e of
  NotIndexed mn muid -> unwords ["Module", moduleNameString mn ++ maybe "" (\uid -> "("++show uid++")") muid, "not indexed."]
  AmbiguousUnitId xs -> unlines $ "UnitId could be any of:" : map ((" - "<>) . unitIdString . modInfoUnit) (toList xs)
    <> ["Use --unit-id to disambiguate"]
  NameNotFound occ mn muid -> unwords
    ["Couldn't find name:", occNameString occ, maybe "" (("from module " ++) . moduleNameString) mn ++ maybe "" (\uid ->"("++show uid++")") muid]
  NameUnhelpfulSpan nm msg -> unwords
    ["Got no helpful spans for:", occNameString (nameOccName nm), "\nMsg:", msg]
 
reportRefSpans :: [(Module,(Int,Int),(Int,Int))] -> IO ()
reportRefSpans = traverse_ $ \(mn,(sl,sc),(el,ec)) ->
  putStrLn $ concat
    [ moduleNameString $ moduleName mn
    , ':':show sl
    , ':':show sc
    , '-':show el
    , ':':show ec
    ]

reportRefs :: [Res RefRow] -> IO ()
reportRefs xs = reportRefSpans
  [ (mdl,(refSLine x, refSCol x),(refELine x, refECol x))
  | (x:.inf) <- xs
  , let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
  ]

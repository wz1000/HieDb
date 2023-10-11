{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module HieDb.Run where

import Prelude hiding (mod)

import GHC
import Compat.HieTypes
import Compat.HieUtils

import qualified Data.Map as M

import qualified Data.Text.IO as T


import System.Environment
import System.Directory
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Exit
import System.Time.Extra

import System.Console.ANSI
import System.Console.Terminal.Size

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Either
import Data.Foldable
import Data.IORef
import Data.List.Extra

import Numeric.Natural

import qualified Data.ByteString.Char8 as BS

import Options.Applicative

import HieDb
import HieDb.Compat
import HieDb.Dump
import Text.Printf (printf)

hiedbMain :: LibDir -> IO ()
hiedbMain libdir = do
  defaultLoc <- getXdgDirectory XdgData $ "default_"++ show dB_VERSION ++".hiedb"
  defdb <- fromMaybe defaultLoc <$> lookupEnv "HIEDB"
  colr <- hSupportsANSIColor stdout
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  (opts, cmd) <- execParser $ progParseInfo defdb colr
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
  , colour :: Bool
  , context :: Maybe Natural
  , reindex :: Bool
  , keepMissing :: Bool
  , srcBaseDir :: Maybe FilePath
  , skipIndexingOptions :: SkipOptions
  }

data Command
  = Init
  | Index [FilePath]
  | NameRefs String (Maybe ModuleName) (Maybe Unit)
  | TypeRefs String (Maybe ModuleName) (Maybe Unit)
  | NameDef  String (Maybe ModuleName) (Maybe Unit)
  | TypeDef  String (Maybe ModuleName) (Maybe Unit)
  | Cat HieTarget
  | Ls
  | LsExports (Maybe ModuleName)
  | Rm [HieTarget]
  | ModuleUIDs ModuleName
  | LookupHieFile ModuleName (Maybe Unit)
  | RefsAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | TypesAtPoint HieTarget (Int,Int) (Maybe (Int,Int))
  | DefsAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | InfoAtPoint  HieTarget (Int,Int) (Maybe (Int,Int))
  | RefGraph
  | Dump FilePath
  | Reachable [Symbol]
  | Unreachable [Symbol]
  | Html [Symbol]
  | GCTypeNames

progParseInfo :: FilePath -> Bool -> ParserInfo (Options, Command)
progParseInfo db colr = info (progParser db colr <**> helper)
  ( fullDesc
  <> progDesc "Query .hie files"
  <> header "hiedb - a tool to query groups of .hie files" )

progParser :: FilePath -> Bool -> Parser (Options,Command)
progParser db colr = (,) <$> optParser db colr <*> cmdParser

optParser :: FilePath -> Bool -> Parser Options
optParser defdb colr
    = Options
  <$> strOption (long "database" <> short 'D' <> metavar "DATABASE"
              <> value defdb <> showDefault <> help "References Database")
  <*> switch (long "trace" <> short 'v' <> help "Print SQL queries being executed")
  <*> switch (long "quiet" <> short 'q' <> help "Don't print progress messages")
  <*> colourFlag
  <*> optional (option auto (long "context" <> short 'C' <> help "Number of lines of context for source spans - show no context by default"))
  <*> switch (long "reindex" <> short 'r' <> help "Re-index all files in database before running command, deleting those with missing '.hie' files")
  <*> switch (long "keep-missing" <> help "Keep missing files when re-indexing")
  <*> optional (strOption (long "src-base-dir" <> help "Provide a base directory to index src files as real files"))
  <*> skipFlags
  where
    colourFlag = flag' True (long "colour" <> long "color" <> help "Force coloured output")
            <|> flag' False (long "no-colour" <> long "no-color" <> help "Force uncoloured ouput")
            <|> pure colr
    skipFlags = do
        refs <- switch (long "skip-refs" <> help "Skip refs table when indexing")
        decls <- switch (long "skip-decls" <> help "Skip decls table when indexing")
        defs <- switch (long "skip-defs" <> help "Skip defs table when indexing")
        exports <- switch (long "skip-exports" <> help "Skip exports table when indexing")
        imports <- switch (long "skip-imports" <> help "Skip imports table when indexing")
        types <- switch (long "skip-types" <> help "Skip types and typerefs table when indexing")
        typeRefs <- switch (long "skip-typerefs" <> help "Skip typerefs table when indexing")
        pure $ SkipOptions
          {
          skipRefs = refs
          , skipDecls = decls
          , skipDefs = defs
          , skipExports = exports
          , skipImports = imports
          , skipTypes = types
          , skipTypeRefs = typeRefs
          }

cmdParser :: Parser Command
cmdParser
   = hsubparser
   $ command "init" (info (pure Init) $ progDesc "Initialize database")
  <> command "index" (info (Index <$> many (strArgument (metavar "DIRECTORY..."))) $ progDesc "Index files from directory")
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
  <> command "ls-exports" (info (LsExports <$> optional moduleNameParser)
                         $ progDesc "List all exports")
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
  <> command "gc" (info (pure GCTypeNames) mempty)

posParser :: Char -> Parser (Int,Int)
posParser c = (,) <$> argument auto (metavar $ c:"LINE") <*> argument auto (metavar $ c:"COL")

maybeUnitId :: Parser (Maybe Unit)
maybeUnitId =
  optional (stringToUnit <$> strOption (short 'u' <> long "unit-id" <> metavar "UNITID"))

symbolParser :: Parser Symbol
symbolParser = argument auto $ metavar "SYMBOL"

moduleNameParser :: Parser ModuleName
moduleNameParser = mkModuleName <$> strArgument (metavar "MODULE")

hieTarget :: Parser HieTarget
hieTarget =
      (Left <$> strOption (long "hiefile" <> short 'f' <> metavar "HIEFILE"))
  <|> (Right <$> ((,) <$> moduleNameParser  <*> maybeUnitId))

progress :: Handle -> Int -> Int -> (FilePath -> DbMonad Bool) -> FilePath -> DbMonad Bool
progress hndl total cur act f = do
  mw <- liftIO $ fmap width <$> size
  let msg' = unwords ["Processing file", show (cur + 1) ++ "/" ++ show total ++ ":", f] ++ "..."
  msg <- liftIO $ case mw of
    Nothing -> hPutStrLn hndl "" >> pure msg'
    Just w -> do
      hPutStr hndl $ replicate w ' '
      hPutStr hndl "\r"
      pure $ take (w-8) msg'
  liftIO $ hPutStr hndl msg
  x <- act f
  if x
  then liftIO $ hPutStr hndl " done\r"
  else liftIO $ hPutStr hndl " skipped\r"
  return x

doIndex :: HieDb -> Options -> Handle -> [FilePath] -> IO ()
doIndex _ opts _ [] | reindex opts = pure ()
doIndex conn opts h files = do
  nc <- newIORef =<< makeNc
  let progress' = if quiet opts then (\_ _ _ k -> k) else progress

  istart <- offsetTime
  (length -> done, length -> skipped)<- runDbM nc $ partition id <$>
    zipWithM (\f n -> progress' h (length files) n (addRefsFrom conn (srcBaseDir opts) (skipIndexingOptions opts)) f) files [0..]
  indexTime <- istart

  start <- offsetTime
  when (done /= 0) $ void $ garbageCollectTypeNames conn
  gcTime <- start

  unless (quiet opts) $
    hPutStrLn h $ "\nCompleted! (" <> show done <> " indexed, " <> show skipped <> " skipped in " <> showDuration indexTime <> " + " <> showDuration gcTime <> " gc)"

runCommand :: LibDir -> Options -> Command -> IO ()
runCommand libdir opts cmd = withHieDbAndFlags libdir (database opts) $ \dynFlags conn -> do
  when (trace opts) $
    setHieTrace conn (Just $ T.hPutStrLn stderr . ("\n****TRACE: "<>))
  when (reindex opts) $ do
    initConn conn
    files' <- map hieModuleHieFile <$> getAllIndexedMods conn
    files <- fmap catMaybes $ forM files' $ \f -> do
      exists <- doesFileExist f
      if exists
      then pure $ Just f
      else do
        unless (keepMissing opts) $
          deleteFileFromIndex conn f
        pure Nothing
    let n = length files
        orig = length files'
    unless (quiet opts) $
      hPutStrLn stderr $ "Re-indexing " ++ show n ++ " files, deleting " ++ show (n-orig) ++ " files"
    doIndex conn opts stderr files
  case cmd of
    Init -> initConn conn
    Index dirs -> do
      initConn conn
      files <- concat <$> mapM getHieFilesIn dirs
      doIndex conn opts stderr files
    TypeRefs typ mn muid -> do
      let occ = mkOccName tcClsName typ
      refs <- findReferences conn False occ mn muid []
      reportRefs opts refs
    NameRefs nm mn muid -> do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      refs <- findReferences conn False occ mn muid []
      reportRefs opts refs
    NameDef nm mn muid -> do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      (row:.inf) <- reportAmbiguousErr opts =<< findOneDef conn occ mn muid
      let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
      reportRefSpans opts [(mdl, (defSLine row, defSCol row), (defELine row, defECol row),Just $ Left (defSrc row))]
    TypeDef nm mn muid -> do
      let occ = mkOccName tcClsName nm
      (row:.inf) <- reportAmbiguousErr opts =<< findOneDef conn occ mn muid
      let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
      reportRefSpans opts [(mdl, (defSLine row, defSCol row), (defELine row, defECol row),Just $ Left (defSrc row))]
    Cat target -> hieFileCommand conn opts target (BS.putStrLn . hie_hs_src)
    Ls -> do
      mods <- getAllIndexedMods conn
      forM_ mods $ \mod -> do
        putStr $ hieModuleHieFile mod
        putStr "\t"
        putStr $ moduleNameString $ modInfoName $ hieModInfo mod
        putStr "\t"
        putStrLn $ unitString $ modInfoUnit $ hieModInfo mod
    LsExports mn -> do
      exports <- maybe (getAllIndexedExports conn) (getExportsForModule conn) mn
      forM_ exports $ \ExportRow{..} -> do
        putStr exportHieFile
        putStr "\t"
        case exportParent of
          Nothing -> putStrLn $ occNameString exportName
          Just p -> printf "%s(%s)\n" (occNameString p) (occNameString exportName)
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
              uid <- reportAmbiguousErr opts =<< maybe (resolveUnitId conn mn) (return . Right) muid
              mFile <- lookupHieFile conn mn uid
              case mFile of
                Nothing -> reportAmbiguousErr opts $ Left (NotIndexed mn $ Just uid)
                Just x -> deleteFileFromIndex conn (hieModuleHieFile x)
    ModuleUIDs mn ->
      print =<< reportAmbiguousErr opts =<< resolveUnitId conn mn
    LookupHieFile mn muid -> reportAmbiguousErr opts =<< do
      euid <- maybe (resolveUnitId conn mn) (return . Right) muid
      case euid of
        Left err -> return $ Left err
        Right uid -> do
          mFile <- lookupHieFile conn mn uid
          case mFile of
            Nothing -> return $ Left (NotIndexed mn $ Just uid)
            Just x -> Right <$> putStrLn (hieModuleHieFile x)
    RefsAtPoint target sp mep -> hieFileCommand conn opts target $ \hf -> do
      let names = concat $ pointCommand hf sp mep $ rights . M.keys . nodeIdentifiers . nodeInfo'
      when (null names) $
        reportAmbiguousErr opts (Left $ NoNameAtPoint target sp)
      forM_ names $ \name -> do
        unless (quiet opts) $ do
          hPutStrLn stderr $ unwords ["Name", ppName opts (nameOccName name),"at",ppSpan opts sp,"is used at:"]
          hPutStrLn stderr ""
        case nameModule_maybe name of
          Just mod -> do
            reportRefs opts =<< findReferences conn False (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod) []
          Nothing -> do
            let refmap = generateReferencesMap (getAsts $ hie_asts hf)
                refs = map (toRef . fst) $ M.findWithDefault [] (Right name) refmap
                toRef spn = (hie_module hf
                            ,(srcSpanStartLine spn , srcSpanStartCol spn)
                            ,(srcSpanEndLine spn , srcSpanEndCol spn)
                            ,Just $ Right (hie_hs_src hf))
            reportRefSpans opts refs
    TypesAtPoint target sp mep -> hieFileCommand conn opts target $ \hf -> do
      let types' = concat $ pointCommand hf sp mep $ nodeType . nodeInfo'
          types = map (flip recoverFullType $ hie_types hf) types'
      when (null types) $
        reportAmbiguousErr opts (Left $ NoNameAtPoint target sp)
      forM_ types $ \typ -> do
        putStrLn $ renderHieType dynFlags typ
    DefsAtPoint target sp mep -> hieFileCommand conn opts target $ \hf -> do
      let names = concat $ pointCommand hf sp mep $ rights . M.keys . nodeIdentifiers . nodeInfo'
      when (null names) $
        reportAmbiguousErr opts (Left $ NoNameAtPoint target sp)
      forM_ names $ \name -> do
        case nameSrcSpan name of
#if __GLASGOW_HASKELL__ >= 900
          RealSrcSpan dsp _ -> do
#else
          RealSrcSpan dsp -> do
#endif
            unless (quiet opts) $
              hPutStrLn stderr $ unwords ["Name", ppName opts (nameOccName name),"at",ppSpan opts sp,"is defined at:"]
            contents <- case nameModule_maybe name of
              Nothing -> pure $ Just $ Right $ hie_hs_src hf
              Just mod
                | mod == hie_module hf -> pure $ Just $ Right $ hie_hs_src hf
                | otherwise -> unsafeInterleaveIO $ do
                    loc <- findOneDef conn (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod)
                    pure $ case loc of
                      Left _ -> Nothing
                      Right (row:._) -> Just $ Left $ defSrc row

            reportRefSpans opts
              [(fromMaybe (hie_module hf) (nameModule_maybe name)
               ,(srcSpanStartLine dsp,srcSpanStartCol dsp)
               ,(srcSpanEndLine dsp, srcSpanEndCol dsp)
               ,contents
               )]
          UnhelpfulSpan msg -> do
            case nameModule_maybe name of
              Just mod -> do
                (row:.inf) <- reportAmbiguousErr opts
                    =<< findOneDef conn (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod)
                unless (quiet opts) $
                  hPutStrLn stderr $ unwords ["Name", ppName opts (nameOccName name),"at",ppSpan opts sp,"is defined at:"]
                reportRefSpans opts
                  [(mkModule (modInfoUnit inf) (modInfoName inf)
                   ,(defSLine row,defSCol row)
                   ,(defELine row,defECol row)
                   ,Just $ Left $ defSrc row
                   )]
              Nothing -> do
                reportAmbiguousErr opts $ Left $ NameUnhelpfulSpan name (unpackFS $ unhelpfulSpanFS msg)
    InfoAtPoint target sp mep -> hieFileCommand conn opts target $ \hf -> do
      mapM_ (uncurry $ printInfo dynFlags) $ pointCommand hf sp mep $ \ast ->
        (hieTypeToIface . flip recoverFullType (hie_types hf) <$> nodeInfo' ast, nodeSpan ast)
    RefGraph -> declRefs conn
    Dump path -> do
      nc <- newIORef =<< makeNc
      runDbM nc $ dump dynFlags path
    Reachable s -> getReachable conn s >>= mapM_ print
    Unreachable s -> getUnreachable conn s >>= mapM_ print
    Html s -> do
      nc <- newIORef =<< makeNc
      runDbM nc $ html conn s
    GCTypeNames -> do
      start <- offsetTime
      n <- garbageCollectTypeNames conn
      end <- start
      unless (quiet opts) $
        hPutStrLn stderr $ "GCed " ++ show n ++ " types in " <> showDuration end

printInfo :: DynFlags -> NodeInfo IfaceType -> RealSrcSpan -> IO ()
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
    putStrLn $ showSDoc dynFlags (ppr typ)
  putStrLn ""

hieFileCommand :: HieDb -> Options -> HieTarget -> (HieFile -> IO a) -> IO a
hieFileCommand conn opts target f = join $ reportAmbiguousErr opts =<< withTarget conn target f

reportAmbiguousErr :: Options -> Either HieDbErr a -> IO a
reportAmbiguousErr _ (Right x) = return x
reportAmbiguousErr o (Left e) = do
  hPutStrLn stderr $ showHieDbErr o e
  exitFailure

showHieDbErr :: Options -> HieDbErr -> String
showHieDbErr opts e = case e of
  NoNameAtPoint t spn -> unwords ["No symbols found at",ppSpan opts spn,"in",either id (\(mn,muid) -> ppMod opts mn ++ maybe "" (\uid -> "("++ppUnit opts uid++")") muid) t]
  NotIndexed mn muid -> unwords ["Module", ppMod opts mn ++ maybe "" (\uid -> "("++ppUnit opts uid++")") muid, "not indexed."]
  AmbiguousUnitId xs -> unlines $ "Unit could be any of:" : map ((" - "<>) . unitString . modInfoUnit) (toList xs)
    <> ["Use --unit-id to disambiguate"]
  NameNotFound occ mn muid -> unwords
    ["Couldn't find name:", ppName opts occ, maybe "" (("from module " ++) . moduleNameString) mn ++ maybe "" (\uid ->"("++ppUnit opts uid++")") muid]
  NameUnhelpfulSpan nm msg -> unwords
    ["Got no helpful spans for:", occNameString (nameOccName nm), "\nMsg:", msg]

reportRefSpans :: Options -> [(Module,(Int,Int),(Int,Int),Maybe (Either FilePath BS.ByteString))] -> IO ()
reportRefSpans opts xs = do
  nc <- newIORef =<< makeNc
  runDbM nc $ forM_ xs $ \(mn,(sl,sc),(el,ec),hie_f) -> do
      liftIO $ do
        when (colour opts) $
          setSGR [SetUnderlining SingleUnderline]
        putStr $ ppMod opts $ moduleName mn
        when (colour opts) $
          setSGR [SetUnderlining SingleUnderline]
        putStr ":"
        when (colour opts) $
          setSGR [SetUnderlining SingleUnderline]
        putStrLn $ colouredPP Magenta id opts $ concat
          [ show sl
          , ':':show sc
          , '-':show el
          , ':':show ec
          ]
        when (colour opts) $
          setSGR []
      case context opts of
        Nothing -> pure ()
        Just (fromIntegral -> n) -> do
          msrc <- forM hie_f $ \case
            Left loc -> withHieFile loc $ pure . hie_hs_src
            Right src -> pure src
          liftIO $ case msrc of
            Nothing -> putStrLn "<source unavailable>"
            Just src -> do
              let ls = BS.lines src

                  (beforeLines',duringLines') = splitAt (sl-1) ls
                  (duringLines,afterLines')   = splitAt (el-sl+1) duringLines'

                  beforeLines = takeEnd n beforeLines'
                  afterLines  = take    n afterLines'

                  (beforeChars,during') = BS.splitAt (sc-1) $ BS.concat $ intersperse "\n" duringLines
                  (during,afterChars) = BS.splitAt (BS.length during' - (BS.length (last duringLines) - ec) - 1) during'

                  before = BS.unlines beforeLines <> beforeChars
                  after  = afterChars <> "\n" <> BS.unlines afterLines

              BS.putStr before
              when (colour opts) $
                setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
              BS.putStr during
              when (colour opts) $
                setSGR []
              BS.putStrLn after

reportRefs :: Options -> [Res RefRow] -> IO ()
reportRefs opts xs = reportRefSpans opts
  [ (mdl,(refSLine x, refSCol x),(refELine x, refECol x),Just $ Left $ refSrc x)
  | (x:.inf) <- xs
  , let mdl = mkModule (modInfoUnit inf) (modInfoName inf)
  ]

colouredPP :: Color -> (a -> String) -> Options -> a -> String
colouredPP c pp opts x = pre <> pp x <> post
  where
    (pre,post)
      | colour opts = (setSGRCode [SetColor Foreground Vivid c], setSGRCode [])
      | otherwise = ("","")


ppName :: Options -> OccName -> String
ppName = colouredPP Red occNameString

ppMod :: Options -> ModuleName -> String
ppMod = colouredPP Green moduleNameString

ppUnit :: Options -> Unit -> String
ppUnit = colouredPP Yellow show

ppSpan :: Options -> (Int,Int) -> String
ppSpan = colouredPP Magenta show

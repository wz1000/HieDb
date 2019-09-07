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
import Outputable ((<+>),hang,showSDoc,ppr,text)

import qualified FastString as FS

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
  | NameDef  String ModuleName (Maybe UnitId)
  | TypeDef  String ModuleName (Maybe UnitId)
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
  | Reachable Symbol
  | Unreachable Symbol

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
                                         <*> maybeUnitId)
                         $ progDesc "Lookup references of value MODULE.NAME")
  <> command "type-refs" (info (TypeRefs <$> (strArgument (metavar "NAME"))
                                         <*> optional moduleNameParser
                                         <*> maybeUnitId)
                         $ progDesc "Lookup references of type MODULE.NAME")
  <> command "name-def" (info (NameDef <$> (strArgument (metavar "NAME"))
                                       <*> moduleNameParser
                                       <*> maybeUnitId)
                         $ progDesc "Lookup definition of value MODULE.NAME")
  <> command "type-def" (info (TypeDef <$> (strArgument (metavar "NAME"))
                                       <*> moduleNameParser
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
                           <*> (posParser 'S')
                           <*> optional (posParser 'E'))
              $ progDesc "Find references for symbol at point/span")
  <> command "point-types"
        (info (TypesAtPoint <$> hieTarget
                            <*> (posParser 'S')
                            <*> optional (posParser 'E'))
              $ progDesc "List types of ast at point/span")
  <> command "point-defs"
        (info (DefsAtPoint <$> hieTarget
                            <*> (posParser 'S')
                            <*> optional (posParser 'E'))
              $ progDesc "Find definition for symbol at point/span")
  <> command "point-info"
        (info (InfoAtPoint <$> hieTarget
                            <*> (posParser 'S')
                            <*> optional (posParser 'E'))
              $ progDesc "Print name, module name, unit id for symbol at point/span")
  <> command "ref-graph" (info (pure RefGraph) $ progDesc "Generate a reachability graph")
  <> command "reachable" (info (Reachable <$> symbolParser)
                         $ progDesc "Find all symbols reachable from a given symbol")
  <> command "unreachable" (info (Unreachable <$> symbolParser)
                           $ progDesc "Find all symbols unreachable from a given symbol")

type HieTarget = Either FilePath (ModuleName,Maybe UnitId)

posParser :: Char -> Parser (Int,Int)
posParser c = ((,) <$> argument auto (metavar $ c:"LINE") <*> argument auto (metavar $ c:"COL"))

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
      evalDbM nc $ sequence_ $
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
    go conn (NameDef nm mn muid) = do
      let ns = if isCons nm then dataName else varName
      let occ = mkOccName ns nm
      (spn, mdl) <- reportAmbiguousErr =<< findDef conn occ mn muid
      reportRefSpans [(mdl, (srcSpanStartLine spn , srcSpanStartCol spn), (srcSpanEndLine spn , srcSpanEndCol spn))]
    go conn (TypeDef nm mn muid) = do
      let occ = mkOccName tcClsName nm
      (spn, mdl) <- reportAmbiguousErr =<< findDef conn occ mn muid
      reportRefSpans [(mdl, (srcSpanStartLine spn , srcSpanStartCol spn), (srcSpanEndLine spn , srcSpanEndCol spn))]
    go conn (Cat target) = hieFileCommand conn target (BS.putStrLn . hie_hs_src)
    go conn Ls = do
      mods <- getAllIndexedMods conn
      forM_ mods $ \mod -> do
        putStr $ hieModuleHieFile mod
        putStr "\t"
        putStr $ moduleNameString $ hieModule mod
        putStr "\t"
        putStrLn $ unitIdString $ hieUnit mod
    go conn (Rm targets) = do
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
              euid <- maybe (resolveUnitId conn mn) (return . Right) muid
              case euid of
                Left err -> reportAmbiguousErr $ Left err
                Right uid -> do
                  mFile <- lookupHieFile conn mn uid
                  case mFile of
                    Nothing -> reportAmbiguousErr $ Left (NotIndexed mn $ Just uid)
                    Just x -> deleteFileFromIndex conn (hieModuleHieFile x)
    go conn (ModuleUIDs mn) = do
      euid <- resolveUnitId conn mn
      case euid of
        Right x -> putStrLn $ show x
        Left (AmbiguousUnitId xs) -> mapM_ (putStrLn . show) xs
        err -> reportAmbiguousErr err >> return ()
    go conn (LookupHieFile mn muid) = reportAmbiguousErr =<< do
      euid <- maybe (resolveUnitId conn mn) (return . Right) muid
      case euid of
        Left err -> return $ Left err
        Right uid -> do
          mFile <- lookupHieFile conn mn uid
          case mFile of
            Nothing -> return $ Left (NotIndexed mn $ Just uid)
            Just x -> Right <$> putStrLn (hieModuleHieFile x)
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
                toRef spn = (hie_module hf,
                              (srcSpanStartLine spn , srcSpanStartCol spn),
                              (srcSpanEndLine spn , srcSpanEndCol spn))
            reportRefSpans refs
    go conn (TypesAtPoint target sp mep) = hieFileCommand conn target $ \hf -> do
      let types' = concat $ pointCommand hf sp mep $ nodeType . nodeInfo
          types = map (flip recoverFullType $ hie_types hf) types'
      dynFlags <- dynFlagsForPrinting
      forM_ types $ \typ -> do
        putStrLn $ renderHieType dynFlags typ
    go conn (DefsAtPoint target sp mep) = hieFileCommand conn target $ \hf -> do
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
                (dsp,mdl) <- reportAmbiguousErr
                    =<< findDef conn (nameOccName name) (moduleName mod) (Just $ moduleUnitId mod)
                putStrLn $ unwords ["Name", occNameString (nameOccName name),"at",show sp,"is defined at:"]
                reportRefSpans [(mdl
                                ,(srcSpanStartLine dsp,srcSpanStartCol dsp)
                                ,(srcSpanEndLine dsp, srcSpanEndCol dsp))]
              Nothing -> do
                reportAmbiguousErr $ Left $ NameUnhelpfulSpan name (FS.unpackFS msg)
    go conn (InfoAtPoint target sp mep) = hieFileCommand conn target $ \hf -> do
      dynFlags <- dynFlagsForPrinting
      mapM_ (uncurry $ printInfo dynFlags) $ pointCommand hf sp mep $ \ast ->
        (renderHieType dynFlags . flip recoverFullType (hie_types hf) <$> nodeInfo ast, nodeSpan ast)
    go conn RefGraph =
      declRefs conn
    go conn (Reachable s) = getReachable conn s >>= mapM_ print
    go conn (Unreachable s) = getUnreachable conn s >>= mapM_ print

printInfo :: DynFlags -> NodeInfo String -> RealSrcSpan -> IO ()
printInfo dynFlags x sp = do
  putStrLn $ "Span: " ++ (showSDoc dynFlags $ ppr sp)
  putStrLn $ "Constructors: " ++ (showSDoc dynFlags $ ppr $ nodeAnnotations x)
  putStrLn "Identifiers:"
  let idents = M.toList $ nodeIdentifiers x
  forM_ idents $ \(ident,inf) -> do
    case ident of
      Left mdl -> putStrLn $ "Module: " ++ moduleNameString mdl
      Right nm -> putStrLn $ showSDoc dynFlags $
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
reportAmbiguousErr (Left (NotIndexed mn muid)) = do
  putStrLn $ unwords $ ["Module",moduleNameString mn ++ maybe "" (\uid -> "("++show uid++")") muid, "not indexed."]
  exitFailure
reportAmbiguousErr (Left (AmbiguousUnitId xs)) = do
  putStrLn $ unwords $ ["UnitId could be any of:",intercalate "," (map show $ toList xs)]
  exitFailure
reportAmbiguousErr (Left (NameNotFound occ mdl)) = do
  putStrLn $ unwords $
    ["Couldn't find name:",occNameString occ,"from module"
    ,moduleNameString (moduleName mdl)++"("++show (moduleUnitId mdl)++")"]
  exitFailure
reportAmbiguousErr (Left (NameUnhelpfulSpan nm msg)) = do
  putStrLn $ unwords $
    ["Got no helpful spans for:", occNameString (nameOccName nm), "\nMsg:", msg]
  exitFailure

reportRefSpans :: [(Module,(Int,Int),(Int,Int))] -> IO ()
reportRefSpans xs = forM_ xs $ \(mn,(sl,sc),(el,ec)) -> do
  putStr (moduleNameString $ moduleName mn)
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
  [ (mdl,(refSLine x, refSCol x),(refELine x, refECol x))
  | x <- xs
  , let mdl = mkModule (refSrcUnit x) (refSrcMod x)
  ]

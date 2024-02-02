{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HieDb.Query where

import           Algebra.Graph.AdjacencyMap (AdjacencyMap, edges, vertexSet, vertices, overlay)
import           Algebra.Graph.Export.Dot hiding ((:=))
import qualified Algebra.Graph.Export.Dot as G

import           GHC
import           Compat.HieTypes

import           System.Directory
import           System.FilePath

import           Control.Monad (foldM, forM_)
import           Control.Monad.IO.Class

import           Data.List (foldl', intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.IORef

import           Database.SQLite.Simple

import           HieDb.Dump (sourceCode)
import           HieDb.Compat
import           HieDb.Types
import           HieDb.Utils
import qualified HieDb.Html as Html

{-| List all modules indexed in HieDb. -}
getAllIndexedMods :: HieDb -> IO [HieModuleRow]
getAllIndexedMods (getConn -> conn) = query_ conn "SELECT * FROM mods"

{-| List all module exports -}
getAllIndexedExports :: HieDb -> IO [(ExportRow)]
getAllIndexedExports (getConn -> conn) = query_ conn "SELECT * FROM exports"

{-| List all exports of the given module -}
getExportsForModule :: HieDb -> ModuleName -> IO [ExportRow]
getExportsForModule (getConn -> conn) mn =
  query conn "SELECT exports.* FROM exports JOIN mods USING (hieFile) WHERE mods.mod = ?" (Only mn)

{-| Find all the modules that export an identifier |-}
findExporters :: HieDb -> OccName -> ModuleName -> Unit -> IO [ModuleName]
findExporters (getConn -> conn) occ mn unit =
  query conn "SELECT mods.mod FROM exports JOIN mods USING (hieFile) WHERE occ = ? AND mod = ? AND unit = ?" (occ, mn, unit)

{-| Lookup Unit associated with given ModuleName.
HieDbErr is returned if no module with given name has been indexed
or if ModuleName is ambiguous (i.e. there are multiple packages containing module with given name)
-}
resolveUnitId :: HieDb -> ModuleName -> IO (Either HieDbErr Unit)
resolveUnitId (getConn -> conn) mn = do
  luid <- query conn "SELECT mod, unit, is_boot, hs_src, is_real, hash FROM mods WHERE mod = ? and is_boot = 0" (Only mn)
  return $ case luid of
    [] ->  Left $ NotIndexed mn Nothing
    [x] -> Right $ modInfoUnit x
    (x:xs) -> Left $ AmbiguousUnitId $ x :| xs

findReferences :: HieDb -> Bool -> OccName -> Maybe ModuleName -> Maybe Unit -> [FilePath] -> IO [Res RefRow]
findReferences (getConn -> conn) isReal occ mn uid exclude =
  queryNamed conn thisQuery ([":occ" := occ, ":mod" := mn, ":unit" := uid, ":real" := isReal] ++ excludedFields)
  where
    excludedFields = zipWith (\n f -> (":exclude" <> T.pack (show n)) := f) [1 :: Int ..] exclude
    thisQuery =
      "SELECT refs.*,mods.mod,mods.unit,mods.is_boot,mods.hs_src,mods.is_real,mods.hash \
      \FROM refs JOIN mods USING (hieFile) \
      \WHERE refs.occ = :occ AND (:mod IS NULL OR refs.mod = :mod) AND (:unit is NULL OR refs.unit = :unit) AND \
            \((NOT :real) OR (mods.is_real AND mods.hs_src IS NOT NULL))"
      <> " AND mods.hs_src NOT IN (" <> Query (T.intercalate "," (map (\(l := _) -> l) excludedFields)) <> ")"

{-| Lookup all 'HieModule' rows from 'HieDb' that are part of a given 'Unit' -}
lookupPackage :: HieDb -> Unit -> IO [HieModuleRow]
lookupPackage (getConn -> conn) uid =
  query conn "SELECT * FROM mods WHERE unit = ?" (Only uid)

{-| Lookup 'HieModule' row from 'HieDb' given its 'ModuleName' and 'Unit' -}
lookupHieFile :: HieDb -> ModuleName -> Unit -> IO (Maybe HieModuleRow)
lookupHieFile (getConn -> conn) mn uid = do
  files <- query conn "SELECT * FROM mods WHERE mod = ? AND unit = ? AND is_boot = 0" (mn, uid)
  case files of
    [] -> return Nothing
    [x] -> return $ Just x
    xs ->
      error $ "DB invariant violated, (mod,unit) in mods not unique: "
            ++ show (moduleNameString mn, uid) ++ ". Entries: "
            ++ intercalate ", " (map hieModuleHieFile xs)

{-| Lookup 'HieModule' row from 'HieDb' given the path to the Haskell source file -}
lookupHieFileFromSource :: HieDb -> FilePath -> IO (Maybe HieModuleRow)
lookupHieFileFromSource (getConn -> conn) fp = do
  files <- query conn "SELECT * FROM mods WHERE hs_src = ?" (Only fp)
  case files of
    [] -> return Nothing
    [x] -> return $ Just x
    xs ->
      error $ "DB invariant violated, hs_src in mods not unique: "
            ++ show fp ++ ". Entries: "
            ++ intercalate ", " (map (show . toRow) xs)

{-| Lookup 'HieModule' row from 'HieDb' given the hash of the HIE file -}
lookupHieFileFromHash :: HieDb -> Fingerprint -> IO (Maybe HieModuleRow)
lookupHieFileFromHash (getConn -> conn) hash = do
  files <- query conn "SELECT * FROM mods WHERE hash = ?" (Only hash)
  case files of
    [] -> return Nothing
    [x] -> return $ Just x
    xs ->
      error $ "DB invariant violated, hash in mods not unique: "
            ++ show hash ++ ". Entries: "
            ++ intercalate ", " (map (show . toRow) xs)

findTypeRefs :: HieDb -> Bool -> OccName -> Maybe ModuleName -> Maybe Unit -> [FilePath] -> IO [Res TypeRef]
findTypeRefs (getConn -> conn) isReal occ mn uid exclude
  = queryNamed conn thisQuery ([":occ" := occ, ":mod" := mn, ":unit" := uid, ":real" := isReal] ++ excludedFields)
  where
    excludedFields = zipWith (\n f -> (":exclude" <> T.pack (show n)) := f) [1 :: Int ..] exclude
    thisQuery =
      "SELECT typerefs.*, mods.mod,mods.unit,mods.is_boot,mods.hs_src,mods.is_real,mods.hash \
      \FROM typerefs JOIN mods ON typerefs.hieFile = mods.hieFile \
                    \JOIN typenames ON typerefs.id = typenames.id \
      \WHERE typenames.name = :occ AND (:mod IS NULL OR typenames.mod = :mod) AND \
            \(:unit IS NULL OR typenames.unit = :unit) AND ((NOT :real) OR (mods.is_real AND mods.hs_src IS NOT NULL))"
      <> " AND mods.hs_src NOT IN (" <> Query (T.intercalate "," (map (\(l := _) -> l) excludedFields)) <> ")"
      <> " ORDER BY typerefs.depth ASC"

findDef :: HieDb -> OccName -> Maybe ModuleName -> Maybe Unit -> IO [Res DefRow]
findDef conn occ mn uid
  = queryNamed (getConn conn) "SELECT defs.*, mods.mod,mods.unit,mods.is_boot,mods.hs_src,mods.is_real,mods.hash \
                              \FROM defs JOIN mods USING (hieFile) \
                              \WHERE occ = :occ AND (:mod IS NULL OR mod = :mod) AND (:unit IS NULL OR unit = :unit)"
                              [":occ" := occ,":mod" := mn, ":unit" := uid]

findOneDef :: HieDb -> OccName -> Maybe ModuleName -> Maybe Unit -> IO (Either HieDbErr (Res DefRow))
findOneDef conn occ mn muid = wrap <$> findDef conn occ mn muid
  where
    wrap [x]    = Right x
    wrap []     = Left $ NameNotFound occ mn muid
    wrap (x:xs) = Left $ AmbiguousUnitId (defUnit x :| map defUnit xs)
    defUnit (_:.i) = i

searchDef :: HieDb -> String -> IO [Res DefRow]
searchDef conn cs
  = query (getConn conn) "SELECT defs.*,mods.mod,mods.unit,mods.is_boot,mods.hs_src,mods.is_real,mods.hash \
                         \FROM defs JOIN mods USING (hieFile) \
                         \WHERE occ LIKE ? \
                         \LIMIT 200" (Only $ '_':':':cs++"%")

{-| @withTarget db t f@ runs function @f@ with HieFile specified by HieTarget @t@.
In case the target is given by ModuleName (and optionally Unit) it is first resolved
from HieDb, which can lead to error if given file is not indexed/Module name is ambiguous.
-}
withTarget
  :: HieDb
  -> HieTarget
  -> (HieFile -> a)
  -> IO (Either HieDbErr a)
withTarget conn target f = case target of
  Left fp -> processHieFile fp
  Right (mn,muid) -> do
    euid <- maybe (resolveUnitId conn mn) (return . Right) muid
    case euid of
      Left err -> return $ Left err
      Right uid -> do
        mModRow <- lookupHieFile conn mn uid
        case mModRow of
          Nothing -> return $ Left (NotIndexed mn $ Just uid)
          Just modRow -> processHieFile (hieModuleHieFile modRow)
  where
    processHieFile fp = do
      fp' <- canonicalizePath fp
      nc <- newIORef =<< makeNc
      runDbM nc $ do
        Right <$> withHieFile fp' (return . f)


type Vertex = (String, String, String, Int, Int, Int, Int)

declRefs :: HieDb -> IO ()
declRefs db = do
  graph <- getGraph db
  writeFile
    "refs.dot"
    ( export
        ( ( defaultStyle ( \( _, hie, occ, _, _, _, _ ) -> hie <> ":" <> occ ) )
          { vertexAttributes = \( mod', _, occ, _, _, _, _ ) ->
              [ "label" G.:= ( mod' <> "." <> drop 1 occ )
              , "fillcolor" G.:= case occ of ('v':_) -> "red"; ('t':_) -> "blue";_ -> "black"
              ]
          }
        )
        graph
    )

getGraph :: HieDb -> IO (AdjacencyMap Vertex)
getGraph (getConn -> conn) = do
  es <-
    query_ conn "SELECT  mods.mod,    decls.hieFile,    decls.occ,    decls.sl,    decls.sc,    decls.el,    decls.ec, \
                       \rmods.mod, ref_decl.hieFile, ref_decl.occ, ref_decl.sl, ref_decl.sc, ref_decl.el, ref_decl.ec \
                \FROM decls JOIN refs              ON refs.hieFile  = decls.hieFile \
                           \JOIN mods              ON mods.hieFile  = decls.hieFile \
                           \JOIN mods  AS rmods    ON rmods.mod = refs.mod AND rmods.unit = refs.unit AND rmods.is_boot = 0 \
                           \JOIN decls AS ref_decl ON ref_decl.hieFile = rmods.hieFile AND ref_decl.occ = refs.occ \
                \WHERE ((refs.sl > decls.sl) OR (refs.sl = decls.sl AND refs.sc >  decls.sc)) \
                  \AND ((refs.el < decls.el) OR (refs.el = decls.el AND refs.ec <= decls.ec))"
  vs <-
    query_ conn "SELECT mods.mod, decls.hieFile, decls.occ, decls.sl, decls.sc, decls.el, decls.ec \
                   \FROM decls JOIN mods USING (hieFile)"
  return $ overlay ( vertices vs ) ( edges ( map (\( x :. y ) -> ( x, y )) es ) )

getVertices :: HieDb -> [Symbol] -> IO [Vertex]
getVertices (getConn -> conn) ss = Set.toList <$> foldM f Set.empty ss
  where
    f :: Set Vertex -> Symbol -> IO (Set Vertex)
    f vs s = foldl' (flip Set.insert) vs <$> one s

    one :: Symbol -> IO [Vertex]
    one s = do
      let n = symName s
          m = moduleNameString $ moduleName $ symModule s
          u = unitString (moduleUnit $ symModule s)
      query conn "SELECT mods.mod, decls.hieFile, decls.occ, decls.sl, decls.sc, decls.el, decls.ec \
                 \FROM decls JOIN mods USING (hieFile) \
                 \WHERE ( decls.occ = ? AND mods.mod = ? AND mods.unit = ? ) " (n, m, u)

getReachable :: HieDb -> [Symbol] -> IO [Vertex]
getReachable db symbols = fst <$> getReachableUnreachable db symbols

getUnreachable :: HieDb -> [Symbol] -> IO [Vertex]
getUnreachable db symbols = snd <$> getReachableUnreachable db symbols

html :: (NameCacheMonad m, MonadIO m) => HieDb -> [Symbol] -> m ()
html db symbols = do
    m <- liftIO $ getAnnotations db symbols
    forM_ (Map.toList m) $ \(fp, (mod', sps)) -> do
        code <- sourceCode fp
        let fp' = replaceExtension fp "html"
        liftIO $ putStrLn $ moduleNameString mod' ++ ": " ++ fp'
        liftIO $ Html.generate fp' mod' code $ Set.toList sps

getAnnotations :: HieDb -> [Symbol] -> IO (Map FilePath (ModuleName, Set Html.Span))
getAnnotations db symbols = do
    (rs, us) <- getReachableUnreachable db symbols
    let m1 = foldl' (f Html.Reachable)   Map.empty rs
        m2 = foldl' (f Html.Unreachable) m1        us
    return m2
  where
    f :: Html.Color
      -> Map FilePath (ModuleName, Set Html.Span)
      -> Vertex
      -> Map FilePath (ModuleName, Set Html.Span)
    f c m v =
        let (fp, mod', sp) = g c v
        in  Map.insertWith h fp (mod', Set.singleton sp) m

    g :: Html.Color -> Vertex -> (FilePath, ModuleName, Html.Span)
    g c (mod', fp, _, sl, sc, el, ec) = (fp, mkModuleName mod', Html.Span
        { Html.spStartLine   = sl
        , Html.spStartColumn = sc
        , Html.spEndLine     = el
        , Html.spEndColumn   = ec
        , Html.spColor       = c
        })

    h :: (ModuleName, Set Html.Span)
      -> (ModuleName, Set Html.Span)
      -> (ModuleName, Set Html.Span)
    h (m, sps) (_, sps') = (m, sps <> sps')

getReachableUnreachable :: HieDb -> [Symbol] -> IO ([Vertex], [Vertex])
getReachableUnreachable db symbols = do
  vs <- getVertices db symbols
  graph  <- getGraph db
  let (xs, ys) = splitByReachability graph vs
  return (Set.toList xs, Set.toList ys)

splitByReachability :: Ord a => AdjacencyMap a -> [a] -> (Set a, Set a)
splitByReachability m vs = let s = Set.fromList (dfs m vs) in (s, vertexSet m Set.\\ s)

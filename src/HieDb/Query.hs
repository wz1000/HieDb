{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HieDb.Query where

import           Algebra.Graph.AdjacencyMap (AdjacencyMap, edges, vertexSet, vertices, overlay)
import           Algebra.Graph.AdjacencyMap.Algorithm (dfs)
import           Algebra.Graph.Export.Dot

import           GHC
import           HieTypes
import           Module
import           Name

import           System.Directory
import           System.FilePath

import           Control.Monad (foldM, forM_)
import           Control.Monad.IO.Class

import           Data.List (foldl', intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Coerce
import           Data.Set (Set)
import qualified Data.Set as Set

import Database.SQLite.Simple hiding ((:=))

import           HieDb.Dump (sourceCode)
import           HieDb.Types
import           HieDb.Utils
import           HieDb.Create
import qualified HieDb.Html as Html

getAllIndexedMods :: HieDb -> IO [HieModuleRow]
getAllIndexedMods (getConn -> conn) = query_ conn "SELECT * FROM mods"

resolveUnitId :: HieDb -> ModuleName -> IO (Either HieDbErr UnitId)
resolveUnitId (getConn -> conn) mn = do
  luid <- query conn "SELECT unit FROM mods WHERE mod = ?" (Only mn)
  case (luid :: [Only UnitId]) of
    [] -> return $ Left $ NotIndexed mn Nothing
    [x] -> return $ Right $ fromOnly x
    (x:xs) -> return $ Left $ AmbiguousUnitId $ coerce $ x :| xs

search :: HieDb -> OccName -> Maybe ModuleName -> Maybe UnitId -> IO [RefRow]
search (getConn -> conn) occ (Just mn) Nothing =
  query conn "SELECT * FROM refs WHERE occ = ? AND mod = ?" (occ, mn)
search (getConn -> conn) occ (Just mn) (Just uid) =
  query conn "SELECT * FROM refs WHERE occ = ? AND mod = ? AND unit = ?" (occ, mn, uid)
search (getConn -> conn) occ _ _=
  query conn "SELECT * FROM refs WHERE occ = ?" (Only occ)

lookupHieFile :: HieDb -> ModuleName -> UnitId -> IO (Maybe HieModuleRow)
lookupHieFile (getConn -> conn) mn uid = do
  files <- query conn "SELECT * FROM mods WHERE mod = ? AND unit = ?" (mn, uid)
  case files of
    [] -> return Nothing
    [x] -> return $ Just x
    xs ->
      error $ "DB invariant violated, (mod,unit) in mods not unique: "
            ++ show (moduleNameString mn, uid) ++ ". Entries: "
            ++ intercalate ", " (map hieModuleHieFile xs)

findDef :: HieDb -> OccName -> ModuleName -> Maybe UnitId -> IO [DefRow]
findDef conn occ mn Nothing
  = query (getConn conn) "SELECT * from defs WHERE occ = ? AND mod = ?" (occ,mn)
findDef conn occ mn (Just uid)
  = query (getConn conn) "SELECT * from defs WHERE occ = ? AND mod = ? AND unit = ?" (occ,mn,uid)

findOneDef :: HieDb -> OccName -> ModuleName -> Maybe UnitId -> IO (Either HieDbErr DefRow)
findOneDef conn occ mn muid = wrap <$> findDef conn occ mn muid
  where
    wrap [x]    = Right x
    wrap []     = Left $ NameNotFound occ mn muid
    wrap (x:xs) = Left $ AmbiguousUnitId (defUnit x :| map defUnit xs)

searchDef :: HieDb -> String -> IO [DefRow]
searchDef conn cs = do
  query (getConn conn) "SELECT * from defs WHERE occ LIKE ?" (Only $ '_':cs++"%")

withTarget
  :: HieDb
  -> Either FilePath (ModuleName, Maybe UnitId)
  -> (HieFile -> a)
  -> IO (Either HieDbErr a)
withTarget conn (Left x') f = do
  x <- canonicalizePath x'
  nc <- makeNc
  evalDbM nc $ do
    addRefsFrom conn x
    Right <$> withHieFile x (return . f)
withTarget conn (Right (mn, muid)) f = do
  euid <- maybe (resolveUnitId conn mn) (return . Right) muid
  case euid of
    Left err -> return $ Left err
    Right uid -> do
      mFile <- lookupHieFile conn mn uid
      case mFile of
        Nothing -> return $ Left (NotIndexed mn $ Just uid)
        Just x -> do
          nc <- makeNc
          evalDbM nc $ do
            file <- liftIO $ canonicalizePath (hieModuleHieFile x)
            addRefsFrom conn file
            Right <$> withHieFile file (return . f)

type Vertex = (String, String, String, Int, Int, Int, Int)

declRefs :: HieDb -> IO ()
declRefs db = do
  graph <- getGraph db
  writeFile
    "refs.dot"
    ( export
        ( ( defaultStyle ( \( _, hie, occ, _, _, _, _ ) -> hie <> ":" <> occ ) )
          { vertexAttributes = \( mod', _, v : occ, _, _, _, _ ) ->
              [ "label" := ( mod' <> "." <> occ )
              , "fillcolor" := case v of 'v' -> "red"; 't' -> "blue" ; _ -> "black" ]
          }
        )
        graph
    )

getGraph :: HieDb -> IO (AdjacencyMap Vertex)
getGraph (getConn -> conn) = do
  es <-
    query_ conn "SELECT decls.mod, decls.hieFile, decls.occ, decls.sl, decls.sc, decls.el, decls.ec, ref_decl.mod, ref_decl.hieFile, ref_decl.occ, ref_decl.sl, ref_decl.sc, ref_decl.el, ref_decl.ec from decls join refs on refs.src = decls.hieFile and refs.srcMod = decls.mod and refs.srcUnit = decls.unit join decls ref_decl on ref_decl.mod = refs.mod and ref_decl.unit = refs.unit and ref_decl.occ = refs.occ where ((refs.sl > decls.sl) or (refs.sl = decls.sl and refs.sc > decls.sc)) and ((refs.el < decls.el) or (refs.el = decls.el and refs.ec <= decls.ec))"
  vs <-
    query_ conn "SELECT decls.mod, decls.hieFile, decls.occ, decls.sl, decls.sc, decls.el, decls.ec from decls"
  return $ overlay ( vertices vs ) ( edges ( map (\( x :. y ) -> ( x, y )) es ) )

getVertices :: HieDb -> [Symbol] -> IO [Vertex]
getVertices (getConn -> conn) ss = Set.toList <$> foldM f Set.empty ss
  where
    f :: Set Vertex -> Symbol -> IO (Set Vertex)
    f vs s = foldl' (flip Set.insert) vs <$> one s

    one :: Symbol -> IO [Vertex]
    one s = do
      let n = toNsChar (occNameSpace $ symName s) : occNameString (symName s)
          m = moduleNameString $ moduleName $ symModule s
          u = unitIdString (moduleUnitId $ symModule s)
      query conn "SELECT mod, hieFile, occ, sl, sc, el, ec FROM decls WHERE ( occ = ? AND mod = ? AND unit = ? ) " (n, m, u)

getReachable :: HieDb -> [Symbol] -> IO [Vertex]
getReachable db symbols = fst <$> getReachableUnreachable db symbols

getUnreachable :: HieDb -> [Symbol] -> IO [Vertex]
getUnreachable db symbols = snd <$> getReachableUnreachable db symbols

html :: HieDb -> [Symbol] -> IO ()
html db symbols = do
    m <- getAnnotations db symbols
    forM_ (Map.toList m) $ \(fp, (mod', sps)) -> do
        code <- sourceCode fp
        let fp' = replaceExtension fp "html"
        putStrLn $ moduleNameString mod' ++ ": " ++ fp'
        Html.generate fp' mod' code $ Set.toList sps

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
splitByReachability m vs = let s = Set.fromList (dfs vs m) in (s, vertexSet m Set.\\ s)

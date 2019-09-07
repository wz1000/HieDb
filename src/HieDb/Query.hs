{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HieDb.Query where

import           Algebra.Graph.AdjacencyMap (AdjacencyMap, edges, postSet, vertexSet, vertices, overlay)
import           Algebra.Graph.AdjacencyMap.Algorithm (dfs)
import           Algebra.Graph.Export.Dot

import           GHC
import           HieTypes
import           Module
import           Name

import           System.Directory

import           Control.Monad.IO.Class

import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Coerce
import           Data.Set (Set)
import qualified Data.Set as Set

import Database.SQLite.Simple hiding ((:=))

import           HieDb.Types
import           HieDb.Utils
import           HieDb.Create

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

findDef :: HieDb -> OccName -> ModuleName -> Maybe UnitId -> IO (Either HieDbErr (RealSrcSpan,Module))
findDef conn occ mn muid = do
  euid <- maybe (resolveUnitId conn mn) (return . Right) muid
  case euid of
    Left err -> return $ Left err
    Right uid -> do
      let mdl = mkModule uid mn
      mres <- lookupHieFile conn mn uid
      case mres of
        Nothing -> do
          -- Module not indexed. Our next best option is to check if any other files in the
          -- unit reference this name. We might be able to get a definition from there
          files <- query (getConn conn)
                     "SELECT DISTINCT src FROM refs WHERE occ = ? AND mod = ? AND unit = ?"
                     (occ,mn,uid)
          maybe (Left $ NameNotFound occ mdl) Right
            <$> tryAll (findDefInFile occ mdl) (map fromOnly files)
        Just modrow -> do
          findDefInFile occ mdl $ hieModuleHieFile modrow

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

type Vertex = (String, String, String)

declRefs :: HieDb -> IO ()
declRefs db = do
  graph <- getGraph db
  writeFile
    "refs.dot"
    ( export
        ( ( defaultStyle ( \( _, hie, occ ) -> hie <> ":" <> occ ) )
          { vertexAttributes = \( mod, _, v : occ ) ->
              [ "label" := ( mod <> "." <> occ )
              , "fillcolor" := case v of 'v' -> "red"; 't' -> "blue" ; _ -> "black" ]
          }
        )
        graph
    )

getGraph :: HieDb -> IO (AdjacencyMap Vertex)
getGraph (getConn -> conn) = do
  es <-
    query_ conn "SELECT decls.mod, decls.hieFile, decls.occ, ref_decl.mod, ref_decl.hieFile, ref_decl.occ from decls join refs on refs.src = decls.hieFile and refs.srcMod = decls.mod and refs.srcUnit = decls.unit join decls ref_decl on ref_decl.mod = refs.mod and ref_decl.unit = refs.unit and ref_decl.occ = refs.occ where ((refs.sl > decls.sl) or (refs.sl = decls.sl and refs.sc > decls.sc)) and ((refs.el < decls.el) or (refs.el = decls.el and refs.ec <= decls.ec))"
  vs <-
    query_ conn "SELECT decls.mod, decls.hieFile, decls.occ from decls"
  return $ overlay ( vertices vs ) ( edges ( map (\( x :. y ) -> ( x, y )) es ) )

getVertices :: HieDb -> Symbol -> IO [Vertex]
getVertices (getConn -> conn) s = do
  let n = toNsChar (occNameSpace $ symName s) : occNameString (symName s)
      m = moduleNameString $ moduleName $ symModule s
      u = unitIdString (moduleUnitId $ symModule s)
  query conn "SELECT mod, hieFile, occ FROM decls WHERE ( occ = ? AND mod = ? AND unit = ? ) OR is_root" (n, m, u)

getReachable :: HieDb -> Symbol -> IO [Vertex]
getReachable db s = do
  vs <- getVertices db s
  graph <- getGraph db
  return $ dfs vs graph

getUnreachable :: HieDb -> Symbol -> IO [Vertex]
getUnreachable db s = do
  vs <- getVertices db s
  graph  <- getGraph db
  let xs = snd $ splitByReachability graph vs
  return $ Set.toList xs

splitByReachability :: Ord a => AdjacencyMap a -> [a] -> (Set a, Set a)
splitByReachability m vs = let s = Set.fromList (dfs vs m) in (s, vertexSet m Set.\\ s)

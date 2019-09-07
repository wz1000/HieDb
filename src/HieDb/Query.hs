{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HieDb.Query where

import           Algebra.Graph.AdjacencyMap (AdjacencyMap, edges, postSet, vertexSet)
import           Algebra.Graph.Export.Dot

import           GHC
import           HieTypes
import           Name

import           System.Directory

import           Control.Monad.IO.Class

import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Coerce
import           Data.Set (Set)
import qualified Data.Set as Set

import           Database.SQLite.Simple

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

type Vertex = (String, Int, Int, Int, Int, String, String, String)

declRefs :: HieDb -> IO ()
declRefs db = do
  graph <- getGraph db
  putStrLn $ export (defaultStyle (\(_, _, _, _, _, f, _, _) -> f)) graph

getGraph :: HieDb -> IO (AdjacencyMap Vertex)
getGraph (getConn -> conn) = do
  drs <-
    query_ conn "SELECt decls.file, decls.sl, decls.sc, decls.el, decls.ec, decls.occ, decls.mod, decls.unit, ref_decl.file, ref_decl.sl, ref_decl.sc, ref_decl.el, ref_decl.ec, ref_decl.occ, ref_decl.mod, ref_decl.unit FROM decls JOIN refs ON refs.srcMod = decls.mod AND refs.srcUnit = decls.unit JOIn decls ref_decl ON ref_decl.mod = refs.mod AND ref_decl.unit = refs.unit AND ref_decl.occ = refs.occ WHERe ((refs.sl > decls.sl) OR (refs.sl = decls.sl AND refs.sc > decls.sc)) AND ((refs.el < decls.el) OR (refs.el = decls.el AND refs.ec <= decls.ec))"
  return $ edges $ map (\( x :. y ) -> ( x, y )) drs

getVertices :: HieDb -> String -> String -> String -> IO [Vertex]
getVertices (getConn -> conn) n m u = do
  xs <- query conn "SELECT file, sl, sc, el, ec FROM decls WHERE occ = ? AND mod = ? AND unit = ?" (n, m, u)
  return $ map (\(f, sl, sc, el, ec) -> (f, sl, sc, el, ec, n, m, u)) xs

getReachable :: HieDb -> String -> String -> String -> IO [Vertex]
getReachable db n m u = do
  vs <- getVertices db n m u
  graph <- getGraph db
  return $ Set.toList $ reachable graph vs

getUnreachable :: HieDb -> String -> String -> String -> IO [Vertex]
getUnreachable db n m u = do
  vs <- getVertices db n m u
  graph  <- getGraph db
  let s = snd $ splitByReachability graph vs
  return $ Set.toList s

reachable :: forall a. Ord a => AdjacencyMap a -> [a] -> Set a
reachable m = go Set.empty
  where
    go :: Set a -> [a] -> Set a
    go s []       = s
    go s (x : xs) = go (Set.insert x s) $ xs ++ [y | y <- Set.toList $ postSet x m, not $ Set.member y s]

splitByReachability :: Ord a => AdjacencyMap a -> [a] -> (Set a, Set a)
splitByReachability m vs = let s = reachable m vs in (s, vertexSet m Set.\\ s)

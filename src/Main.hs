module Main where

import GHC
import OccName
import HieTypes
import HieUtils
import Name
import NameCache
import HieBin
import UniqSupply


import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.IO.Class

import Data.Coerce

newtype RefMap =
  RefMap { getRefMap :: Map (OccName,ModuleName) (Set Span) }

instance Semigroup RefMap where
  (<>) = coerce $ M.unionWith (S.union)

instance Monoid RefMap where
  mempty = RefMap (M.empty)

type DbMonad a = StateT NameCache (WriterT RefMap IO) a

genRefMap :: HieFile -> RefMap
genRefMap hf = toRefMap $ generateReferencesMap $ getAsts $ hie_asts hf
  where
    toRefMap = RefMap . M.fromList . go . M.toList
    go = foldr go' []
    go' (Right name,dets) acc
      | Just mod <- nameModule_maybe name =
          ((nameOccName name,moduleName mod), S.fromList $ map fst dets):acc
    go' _ acc = acc

collectReferences :: FilePath -> DbMonad ()
collectReferences path = do
  nc <- get
  (hiefile, nc') <- liftIO $ readHieFile nc path
  put nc'
  tell $ genRefMap hiefile

collectAllReferences :: [FilePath] -> IO RefMap
collectAllReferences xs = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  execWriterT $ flip evalStateT nc
              $ mapM_ collectReferences xs

-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <- doesPathExist path
  if exists then do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile && ("hie" `isExtensionOf` path) then do
      path' <- makeAbsolute path
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

main :: IO ()
main = do
  args <- getArgs
  hSetBuffering stdout NoBuffering
  let conf = parseConf args
  fs <- getHieFilesIn (in_dir conf)
  putStr "Collecting data..."
  refmap <- collectAllReferences fs
  putStrLn " done"
  interactive refmap

printRefs :: Set Span -> IO ()
printRefs sps = do
  putStrLn "Referred to in: "
  mapM_ print sps

interactive :: RefMap -> IO ()
interactive rf@(RefMap m) = do
  putStrLn "\t 1) Type/Type Class"
  putStrLn "\t 2) Data Constructor"
  putStrLn "\t 3) Var"
  putStr "Choose Name Space(default 3): "
  n <- getLine
  let ns = case reads n of
        (1,_):_ -> tcClsName
        (2,_):_ -> dataName
        _ -> varName
  putStr "Enter module: "
  mod <- getLine
  putStr "Enter name: "
  name <- getLine

  case M.lookup (mkOccName ns name, mkModuleName mod) m of
    Just refs -> printRefs refs
    Nothing -> putStrLn "Not found"

  putStr "q to quit, otherwise continue"
  c <- getLine
  when (c /= "q") $ interactive rf

parseConf :: [String] -> HieDbConf
parseConf [dir,out] = HieDbConf dir out
parseConf ("-d":dir:xs) = (parseConf xs){in_dir = dir}
parseConf ("-o":file:xs) = (parseConf xs){ofile = file}
parseConf (dir:xs) = (parseConf xs){in_dir = dir}
parseConf _ = HieDbConf "." "./out.hiedb"

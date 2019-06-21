{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module HieDb.Utils where

import Prelude hiding (mod)

import HieBin
import HieTypes
import HieUtils
import Name
import Module
import NameCache
import UniqSupply
import SrcLoc
import DynFlags
import SysTools
import GHC.Paths (libdir)

import qualified Data.Map as M

import qualified FastString as FS

import System.Directory
import System.FilePath

import Control.Monad.IO.Class

import Data.Char
import Data.Maybe

import HieDb.Types

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

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

withHieFile :: (NameCacheMonad m, MonadIO m)
            => FilePath
            -> (HieFile -> m a)
            -> m a
withHieFile path act = do
  nc <- getNc
  (hiefile, nc') <- liftIO $ readHieFile nc path
  putNc nc'
  act (hie_file_result hiefile)

tryAll :: Monad m => (a -> m (Either b c)) -> [a] -> m (Maybe c)
tryAll _ [] = return Nothing
tryAll f (x:xs) = do
  eres <- f x
  case eres of
    Right res -> return (Just res)
    Left _ -> tryAll f xs

-- | Given the path to a HieFile, it tries to find the SrcSpan of an External name in
-- it by loading it and then looking for the name in NameCache
findDefInFile :: OccName -> Module -> FilePath -> IO (Either HieDbErr (RealSrcSpan,Module))
findDefInFile occ mdl file = do
  nc <- makeNc
  nc' <- execDbM nc $ withHieFile file (const $ return ())
  case lookupOrigNameCache (nsNames nc') mdl occ of
    Just name -> case nameSrcSpan name of
      RealSrcSpan sp -> return $ Right (sp, mdl)
      UnhelpfulSpan msg -> return $ Left $ NameUnhelpfulSpan name (FS.unpackFS msg)
    Nothing -> return $ Left $ NameNotFound occ mdl

pointCommand :: HieFile -> (Int, Int) -> Maybe (Int, Int) -> (HieAST TypeIndex -> a) -> [a]
pointCommand hf (sl,sc) mep k =
    catMaybes $ M.elems $ flip M.mapWithKey (getAsts $ hie_asts hf) $ \fs ast ->
      case selectSmallestContaining (sp fs) ast of
        Nothing -> Nothing
        Just ast' -> Just $ k ast'
 where
   sloc fs = mkRealSrcLoc fs sl sc
   eloc fs = case mep of
     Nothing -> sloc fs
     Just (el,ec) -> mkRealSrcLoc fs el ec
   sp fs = mkRealSrcSpan (sloc fs) (eloc fs)

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])

isCons :: String -> Bool
isCons (':':_) = True
isCons (x:_) | isUpper x = True
isCons _ = False

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
          RefRow path smod sunit occ (moduleName mod) (moduleUnitId mod) file sl sc el ec
          where
            smod = moduleName $ hie_module hf
            sunit = moduleUnitId $ hie_module hf
            occ = nameOccName name
            file = FS.unpackFS $ srcSpanFile sp
            sl = srcSpanStartLine sp
            sc = srcSpanStartCol sp
            el = srcSpanEndLine sp
            ec = srcSpanEndCol sp
    go _ = Nothing


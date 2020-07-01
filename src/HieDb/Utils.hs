{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module HieDb.Utils where

import qualified Data.Tree

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


#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Doc
#else
import HsDoc
#endif

import qualified Data.Map as M

import qualified FastString as FS

import System.Directory
import System.FilePath

import Control.Arrow ( (&&&) )
import Data.Bifunctor ( bimap )
import Data.List (find)
import Control.Monad.IO.Class

import Data.Char
import Data.Maybe
import Data.Monoid

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
    Nothing -> return $ Left $ NameNotFound occ (Just $ moduleName mdl) (Just $ moduleUnitId mdl)

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
#if __GLASGOW_HASKELL__ >= 810
  return $ defaultDynFlags systemSettings $ LlvmConfig [] []
#else
  return $ defaultDynFlags systemSettings ([], [])
#endif

isCons :: String -> Bool
isCons (':':_) = True
isCons (x:_) | isUpper x = True
isCons _ = False

genRefsAndDecls :: FilePath -> Module -> M.Map Identifier [(Span, IdentifierDetails a)] -> ([RefRow],[DeclRow])
genRefsAndDecls path smdl refmap = genRows $ flat $ M.toList refmap
  where
    flat = concatMap (\(a,xs) -> map (a,) xs)
    genRows = foldMap go
    go = bimap maybeToList maybeToList . (goRef &&& goDec)

    goRef (Right name, (sp,_))
      | Just mod <- nameModule_maybe name = Just $
          RefRow path occ (moduleName mod) (moduleUnitId mod) file sl sc el ec
          where
            occ = nameOccName name
            file = FS.unpackFS $ srcSpanFile sp
            sl = srcSpanStartLine sp
            sc = srcSpanStartCol sp
            el = srcSpanEndLine sp
            ec = srcSpanEndCol sp
    goRef _ = Nothing

    goDec (Right name,(_,dets))
      | Just mod <- nameModule_maybe name
      , mod == smdl
      , occ  <- nameOccName name
      , info <- identInfo dets
      , Just sp <- getBindSpan info
      , is_root <- isRoot info
      , file <- FS.unpackFS $ srcSpanFile sp
      , sl   <- srcSpanStartLine sp
      , sc   <- srcSpanStartCol sp
      , el   <- srcSpanEndLine sp
      , ec   <- srcSpanEndCol sp
      = Just $ DeclRow path occ file sl sc el ec is_root
    goDec _ = Nothing

    isRoot = any (\case
      ValBind InstanceBind _ _ -> True
      Decl _ _ -> True
      _ -> False)

    getBindSpan = getFirst . foldMap (First . goDecl)
    goDecl (ValBind _ _ sp) = sp
    goDecl (PatternBind _ _ sp) = sp
    goDecl (Decl _ sp) = sp
    goDecl (RecField _ sp) = sp
    goDecl _ = Nothing

genDefRow :: FilePath -> Module -> DeclDocMap -> M.Map Identifier [(Span, IdentifierDetails a)] -> (a -> String) -> [DefRow]
genDefRow path smod (DeclDocMap docmap) refmap printType = genRows $ M.toList refmap
  where
    genRows = mapMaybe go
    getSpan name dets
      | RealSrcSpan sp <- nameSrcSpan name = Just sp
      | otherwise = do
          (sp, dets) <- find defSpan dets
          pure sp

    defSpan = any isDef . identInfo . snd
    isDef (ValBind RegularBind _ _) = True
    isDef PatternBind{}             = True
    isDef Decl{}                    = True
    isDef _                         = False

    go ((Right name),dets)
      | Just mod <- nameModule_maybe name
      , mod == smod
      , occ  <- nameOccName name
      , Just sp <- getSpan name dets
      , file <- FS.unpackFS $ srcSpanFile sp
      , sl   <- srcSpanStartLine sp
      , sc   <- srcSpanStartCol sp
      , el   <- srcSpanEndLine sp
      , ec   <- srcSpanEndCol sp
      , let typ = getFirst $ foldMap (First . identType . snd) dets
      = Just $ DefRow path occ file sl sc el ec (printType <$> typ) (unpackHDS <$> M.lookup name docmap)
    go _ = Nothing

identifierTree :: HieTypes.HieAST a -> Data.Tree.Tree ( HieTypes.HieAST a )
identifierTree HieTypes.Node{ nodeInfo, nodeSpan, nodeChildren } =
  Data.Tree.Node
    { rootLabel = HieTypes.Node{ nodeInfo, nodeSpan, nodeChildren = mempty }
    , subForest = map identifierTree nodeChildren
    }

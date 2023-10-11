{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module HieDb.Utils where

import qualified Data.Tree

import Prelude hiding (mod)

import Compat.HieBin
import Compat.HieTypes
import qualified Compat.HieTypes as HieTypes
import Compat.HieUtils
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S


import System.Directory
import System.FilePath

import Data.List (find)
import Control.Monad.IO.Class
import qualified Data.Array as A

import Data.Char
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.IORef

import HieDb.Types
import HieDb.Compat
import Database.SQLite.Simple

#if __GLASGOW_HASKELL__ >= 903
import Control.Concurrent.MVar (readMVar)
#endif

addTypeRef :: HieDb -> FilePath -> A.Array TypeIndex HieTypeFlat -> A.Array TypeIndex (Maybe Int64) -> RealSrcSpan -> TypeIndex -> IO ()
addTypeRef (getConn -> conn) hf arr ixs sp = go 0
  where
    sl = srcSpanStartLine sp
    sc = srcSpanStartCol sp
    el = srcSpanEndLine sp
    ec = srcSpanEndCol sp
    go :: TypeIndex -> Int -> IO ()
    go d i = do
      case ixs A.! i of
        Nothing -> pure ()
        Just occ -> do
          let ref = TypeRef occ hf d sl sc el ec
          execute conn "INSERT INTO typerefs VALUES (?,?,?,?,?,?,?)" ref
      let next = go (d+1)
      case arr A.! i of
        HTyVarTy _ -> pure ()
#if __GLASGOW_HASKELL__ >= 808
        HAppTy x (HieArgs xs) -> mapM_ next (x:map snd xs)
#else
        HAppTy x y -> mapM_ next [x,y]
#endif
        HTyConApp _ (HieArgs xs) -> mapM_ (next . snd) xs
        HForAllTy ((_ , a),_) b -> mapM_ next [a,b]
#if __GLASGOW_HASKELL__ >= 900
        HFunTy a b c -> mapM_ next [a,b,c]
#else
        HFunTy a b -> mapM_ next [a,b]
#endif
        HQualTy a b -> mapM_ next [a,b]
        HLitTy _ -> pure ()
        HCastTy a -> go d a
        HCoercionTy -> pure ()

makeNc :: IO NameCache
makeNc = do
#if __GLASGOW_HASKELL__ >= 903
  initNameCache 'z' []
#else
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []
#endif

-- | Recursively search for @.hie@ and @.hie-boot@  files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  isFile <- doesFileExist path
  if isFile && ("hie" `isExtensionOf` path || "hie-boot" `isExtensionOf` path) then do
      path' <- canonicalizePath path
      return [path']
  else do
    isDir <- doesDirectoryExist path
    if isDir then do
      cnts <- listDirectory path
      withCurrentDirectory path $ foldMap getHieFilesIn cnts
    else
      return []

withHieFile :: (NameCacheMonad m, MonadIO m)
            => FilePath
            -> (HieFile -> m a)
            -> m a
withHieFile path act = do
  ncu <- getNcUpdater
  hiefile <- liftIO $ readHieFile ncu path
  act (hie_file_result hiefile)

-- | Given the path to a HieFile, it tries to find the SrcSpan of an External name in
-- it by loading it and then looking for the name in NameCache
findDefInFile :: OccName -> Module -> FilePath -> IO (Either HieDbErr (RealSrcSpan,Module))
findDefInFile occ mdl file = do
  ncr <- newIORef =<< makeNc
  _ <- runDbM ncr $ withHieFile file (const $ return ())
  nc <- readIORef ncr
#if __GLASGOW_HASKELL__ >= 903
  nsns <- readMVar (nsNames nc)
#else
  let nsns = nsNames nc
#endif
  return $ case lookupOrigNameCache nsns mdl occ of
    Just name -> case nameSrcSpan name of
#if __GLASGOW_HASKELL__ >= 900
      RealSrcSpan sp _ -> Right (sp, mdl)
#else
      RealSrcSpan sp -> Right (sp, mdl)
#endif
      UnhelpfulSpan msg -> Left $ NameUnhelpfulSpan name (unpackFS $ unhelpfulSpanFS msg)
    Nothing -> Left $ NameNotFound occ (Just $ moduleName mdl) (Just $ moduleUnit mdl)

pointCommand :: HieFile -> (Int, Int) -> Maybe (Int, Int) -> (HieAST TypeIndex -> a) -> [a]
pointCommand hf (sl,sc) mep k =
    M.elems $ flip M.mapMaybeWithKey (getAsts $ hie_asts hf) $ \fs ast ->
      k <$> selectSmallestContaining (sp $ hiePathToFS fs) ast
 where
   sloc fs = mkRealSrcLoc fs sl sc
   eloc fs = case mep of
     Nothing -> sloc fs
     Just (el,ec) -> mkRealSrcLoc fs el ec
   sp fs = mkRealSrcSpan (sloc fs) (eloc fs)

dynFlagsForPrinting :: LibDir -> IO DynFlags
dynFlagsForPrinting (LibDir libdir) = do
  systemSettings <- initSysTools
#if __GLASGOW_HASKELL__ >= 808
                    libdir
#else
                    (Just libdir)
#endif
#if __GLASGOW_HASKELL__ >= 905
  return $ defaultDynFlags systemSettings
#elif __GLASGOW_HASKELL__ >= 810
  return $ defaultDynFlags systemSettings $ LlvmConfig [] []
#else
  return $ defaultDynFlags systemSettings ([], [])
#endif

isCons :: String -> Bool
isCons (':':_) = True
isCons (x:_) | isUpper x = True
isCons _ = False

data AstInfo =
  AstInfo
    { astInfoRefs :: [RefRow]
    , astInfoDecls :: [DeclRow]
    , astInfoImports :: [ImportRow]
    }

instance Semigroup AstInfo where 
  AstInfo r1 d1 i1 <> AstInfo r2 d2 i2 = AstInfo (r1 <> r2) (d1 <> d2) (i1 <> i2)

instance Monoid AstInfo where 
  mempty = AstInfo [] [] []

genAstInfo :: FilePath -> Module -> M.Map Identifier [(Span, IdentifierDetails a)] -> AstInfo
genAstInfo path smdl refmap = genRows $ flat $ M.toList refmap
  where
    flat = concatMap (\(a,xs) -> map (a,) xs)
    genRows = foldMap go
    go = mkAstInfo

    mkAstInfo x = AstInfo (maybeToList $ goRef x) (maybeToList $ goDec x) (maybeToList $ goImport x)

    goRef (Right name, (sp,_))
      | Just mod <- nameModule_maybe name = Just $
          RefRow path occ (moduleName mod) (moduleUnit mod) sl sc el ec
          where
            occ = nameOccName name
            sl = srcSpanStartLine sp
            sc = srcSpanStartCol sp
            el = srcSpanEndLine sp
            ec = srcSpanEndCol sp
    goRef _ = Nothing

    goImport (Left modName, (sp, IdentifierDetails _ contextInfos)) = do
          _ <- guard $ not $ S.disjoint contextInfos $ S.fromList [IEThing Import, IEThing ImportAs, IEThing ImportHiding]
          let
            sl = srcSpanStartLine sp
            sc = srcSpanStartCol sp
            el = srcSpanEndLine sp
            ec = srcSpanEndCol sp
          Just $ ImportRow path modName sl sc el ec
    goImport _ = Nothing

    goDec (Right name,(_,dets))
      | Just mod <- nameModule_maybe name
      , mod == smdl
      , occ  <- nameOccName name
      , info <- identInfo dets
      , Just sp <- getBindSpan info
      , is_root <- isRoot info
      , sl   <- srcSpanStartLine sp
      , sc   <- srcSpanStartCol sp
      , el   <- srcSpanEndLine sp
      , ec   <- srcSpanEndCol sp
      = Just $ DeclRow path occ sl sc el ec is_root
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

genDefRow :: FilePath -> Module -> M.Map Identifier [(Span, IdentifierDetails a)] -> [DefRow]
genDefRow path smod refmap = genRows $ M.toList refmap
  where
    genRows = mapMaybe go
    getSpan name dets
#if __GLASGOW_HASKELL__ >= 900
      | RealSrcSpan sp _ <- nameSrcSpan name = Just sp
#else
      | RealSrcSpan sp <- nameSrcSpan name = Just sp
#endif
      | otherwise = do
          (sp, _dets) <- find defSpan dets
          pure sp

    defSpan = any isDef . identInfo . snd
    isDef (ValBind RegularBind _ _) = True
    isDef PatternBind{}             = True
    isDef Decl{}                    = True
    isDef _                         = False

    go (Right name,dets)
      | Just mod <- nameModule_maybe name
      , mod == smod
      , occ  <- nameOccName name
      , Just sp <- getSpan name dets
      , sl   <- srcSpanStartLine sp
      , sc   <- srcSpanStartCol sp
      , el   <- srcSpanEndLine sp
      , ec   <- srcSpanEndCol sp
      = Just $ DefRow path occ sl sc el ec
    go _ = Nothing

identifierTree :: HieTypes.HieAST a -> Data.Tree.Tree ( HieTypes.HieAST a )
identifierTree nd@HieTypes.Node{ nodeChildren } =
  Data.Tree.Node
    { rootLabel = nd { nodeChildren = mempty }
    , subForest = map identifierTree nodeChildren
    }

generateExports :: FilePath -> [AvailInfo] -> [ExportRow]
generateExports fp = concatMap generateExport where
  generateExport :: AvailInfo -> [ExportRow]
  generateExport (AvailName n)
    = [ExportRow
        { exportHieFile = fp
        , exportName = nameOccName n
        , exportMod = moduleName $ nameModule n
        , exportUnit = moduleUnit $ nameModule n
        , exportParent = Nothing
        , exportParentMod = Nothing
        , exportParentUnit = Nothing
        , exportIsDatacon = False
        }]
  generateExport (AvailFL fl)
    = [ExportRow
        { exportHieFile = fp
        , exportName = n
        , exportMod = m
        , exportUnit = u
        , exportParent = Nothing
        , exportParentMod = Nothing
        , exportParentUnit = Nothing
        , exportIsDatacon = False
        }]
      where
        (n, m, u) = (mkVarOccFS $ field_label $ flLabel fl
                    -- For fields, the module details come from the parent
                    ,moduleName $ nameModule $ flSelector fl
                    ,moduleUnit $ nameModule $ flSelector fl
                    )
  generateExport (AvailTC name pieces fields)
    = ExportRow
        { exportHieFile = fp
        , exportName = nameOccName name
        , exportMod = moduleName $ nameModule name
        , exportUnit = moduleUnit $ nameModule name
        , exportParent = Nothing
        , exportParentMod = Nothing
        , exportParentUnit = Nothing
        , exportIsDatacon = False}
    : [ExportRow
        { exportHieFile = fp
        , exportName = n
        , exportMod = m
        , exportUnit = u
        , exportParent = Just (nameOccName name)
        , exportParentMod = Just (moduleName $ nameModule name)
        , exportParentUnit = Just (moduleUnit $ nameModule name)
        , exportIsDatacon = False}
      | (n,m,u) <- map (\n ->
                        (nameOccName n
                        ,moduleName $ nameModule n
                        ,moduleUnit $ nameModule n
                        ))
                      (drop 1 pieces)
               <> map (\s ->
                        (mkVarOccFS $ field_label $ flLabel s
                        -- For fields, the module details come from the parent
                        ,moduleName $ nameModule name
                        ,moduleUnit $ nameModule name
                        ))
                      fields
      ]

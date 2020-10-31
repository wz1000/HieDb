{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}

module HieDb.Dump where

import qualified Data.Map.Strict as Map
import qualified Compat.HieBin as HieBin
import           Compat.HieBin ( HieFileResult( HieFileResult ) )
import           HieDb.Utils ( dynFlagsForPrinting )
import qualified Compat.HieDebug as HieDebug
import qualified Compat.HieTypes as HieTypes
import           Compat.HieTypes ( HieFile(..) )
import qualified NameCache
import qualified Outputable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified UniqSupply
import Data.IORef

dump :: FilePath -> FilePath -> IO ()
dump libdir hieFilePath = do
  nameCache <- do
    uniqueSupply <-
      UniqSupply.mkSplitUniqSupply 'z'

    newIORef ( NameCache.initNameCache uniqueSupply [] )

  HieBin.HieFileResult{ hie_file_result } <-
    HieBin.readHieFile (HieBin.NCU $ atomicModifyIORef' nameCache) hieFilePath

  let
    HieFile{ hie_asts } =
      hie_file_result

    astRoot =
      HieTypes.getAsts hie_asts
        Map.! head ( Map.keys ( HieTypes.getAsts hie_asts ) )

  dynFlags <-
    dynFlagsForPrinting libdir

  putStrLn
    ( Outputable.showSDoc
        dynFlags
        ( HieDebug.ppHie astRoot )
    )

sourceCode :: FilePath -> IO [Text]
sourceCode hieFilePath = do
  nameCache <- do
    uniqueSupply <-
      UniqSupply.mkSplitUniqSupply 'z'

    newIORef ( NameCache.initNameCache uniqueSupply [] )

  HieBin.HieFileResult{ hie_file_result } <-
    HieBin.readHieFile (HieBin.NCU $ atomicModifyIORef' nameCache) hieFilePath

  return $ T.lines $ T.decodeUtf8 $ hie_hs_src hie_file_result

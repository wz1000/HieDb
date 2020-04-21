{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}

module HieDb.Dump where

import qualified Data.Map.Strict as Map
import qualified HieBin
import           HieBin ( HieFileResult( HieFileResult ) )
import           HieDb.Utils ( dynFlagsForPrinting )
import qualified HieDebug
import qualified HieTypes
import           HieTypes ( HieFile(..) )
import qualified NameCache
import qualified Outputable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified UniqSupply

dump :: FilePath -> IO ()
dump hieFilePath = do
  nameCache <- do
    uniqueSupply <-
      UniqSupply.mkSplitUniqSupply 'z'

    return ( NameCache.initNameCache uniqueSupply [] )

  ( HieBin.HieFileResult{ hie_file_result }, _ ) <-
    HieBin.readHieFile nameCache hieFilePath

  let
    HieFile{ hie_asts } =
      hie_file_result

    astRoot =
      HieTypes.getAsts hie_asts
        Map.! head ( Map.keys ( HieTypes.getAsts hie_asts ) )

  dynFlags <-
    dynFlagsForPrinting

  putStrLn
    ( Outputable.showSDoc
        dynFlags
        ( Outputable.ppr astRoot )
    )

sourceCode :: FilePath -> IO [Text]
sourceCode hieFilePath = do
  nameCache <- do
    uniqueSupply <-
      UniqSupply.mkSplitUniqSupply 'z'

    return ( NameCache.initNameCache uniqueSupply [] )

  ( HieBin.HieFileResult{ hie_file_result }, _ ) <-
    HieBin.readHieFile nameCache hieFilePath

  return $ T.lines $ T.decodeUtf8 $ hie_hs_src hie_file_result

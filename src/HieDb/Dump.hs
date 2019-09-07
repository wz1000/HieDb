{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module HieDb.Dump where

import Control.Monad ( guard )
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Set
import qualified DynFlags
import qualified FastString
import qualified GHC
import GHC.Paths (libdir)
import qualified HieBin
import HieBin ( HieFileResult( HieFileResult ) )
import HieDb.Utils ( dynFlagsForPrinting )
import qualified HieDebug
import qualified HieTypes
import HieTypes ( HieFile( HieFile ) )
import qualified Name
import qualified NameCache
import qualified Outputable
import SysTools ( initSysTools )
import System.Environment ( getArgs )
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
    HieFile{ hie_hs_file, hie_asts } =
      hie_file_result

    astRoot =
      HieTypes.getAsts hie_asts
        Map.! head ( Map.keys ( HieTypes.getAsts hie_asts ) )

  dynFlags <-
    dynFlagsForPrinting

  putStrLn
    ( Outputable.showSDoc
        dynFlags
        ( HieDebug.ppHie astRoot )
    )

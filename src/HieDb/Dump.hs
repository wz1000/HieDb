{-# LANGUAGE NamedFieldPuns #-}

module HieDb.Dump where

import qualified Compat.HieDebug as HieDebug
import qualified Compat.HieTypes as HieTypes
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Outputable

import           Compat.HieTypes (HieFile (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import           DynFlags (DynFlags)
import           HieDb.Types (NameCacheMonad)
import           HieDb.Utils (withHieFile)

{-| Pretty print Hie AST stored in given .hie file -}
dump ::
    (NameCacheMonad m, MonadIO m)
    => DynFlags
    -> FilePath -- ^ Path to .hie file
    -> m ()
dump dynFlags hieFilePath = do
  withHieFile hieFilePath $ \HieFile{ hie_asts } -> do
    let (_, astRoot) = Map.findMin $ HieTypes.getAsts hie_asts
    liftIO $ putStrLn $ Outputable.showSDoc dynFlags $ HieDebug.ppHie astRoot

{-| Get lines of original source code from given .hie file -}
sourceCode :: (NameCacheMonad m, MonadIO m) => FilePath -> m [Text]
sourceCode hieFilePath =
  withHieFile hieFilePath $ \HieFile {hie_hs_src} ->
    return $ T.lines $ T.decodeUtf8 hie_hs_src

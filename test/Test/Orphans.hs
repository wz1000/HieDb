{-# LANGUAGE StandaloneDeriving, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Orphans where

import HieDb.Compat
import HieDb.Types

#if __GLASGOW_HASKELL__ < 902
instance Show ModuleName where show = moduleNameString
#endif
instance Show OccName where show = occNameString
instance Show Name where
  show n =
    let occ = nameOccName n
        mod' = nameModule n
        mn = moduleName mod'
        uid = moduleUnit mod'
    in show uid <> ":" <> show mn <> ":" <> show occ

deriving instance Show HieDbErr

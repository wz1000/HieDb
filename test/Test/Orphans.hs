{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Orphans where

import HieDb.Compat
import HieDb.Types

instance Show OccName where show = occNameString
instance Show Name where
  show n =
    let occ = nameOccName n
        mod' = nameModule n
        mn = moduleName mod'
        uid = moduleUnit mod'
    in show uid <> ":" <> show mn <> ":" <> show occ

deriving instance Show HieDbErr

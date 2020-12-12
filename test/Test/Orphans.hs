{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Orphans where

import HieDb.Types
import Module (ModuleName, moduleName, moduleNameString, moduleUnitId)
import Name (Name, nameModule, nameOccName)
import OccName (OccName, occNameString)

instance Show ModuleName where show = moduleNameString
instance Show OccName where show = occNameString
instance Show Name where
  show n =
    let occ = nameOccName n
        mod' = nameModule n
        mn = moduleName mod'
        uid = moduleUnitId mod'
    in show uid <> ":" <> show mn <> ":" <> show occ

deriving instance Show HieDbErr

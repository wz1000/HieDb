module Main where

import GHC.Paths (libdir)
import HieDb.Run (hiedbMain)
import HieDb.Types (LibDir (..))

main :: IO ()
main = hiedbMain (LibDir libdir)

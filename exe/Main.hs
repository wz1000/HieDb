module Main where

import HieDb.Run
import GHC.Paths (libdir)

main :: IO ()
main = hiedbMain libdir

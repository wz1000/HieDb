{-# LANGUAGE DuplicateRecordFields #-}
module Sub.Module2
    ( showInt
    , Data1(..)
    ) where

showInt :: Int -> String
showInt = show

data Data1
    = Data1Constructor1
    | Data1Constructor2 Int

data Data2
    = Data2C { foo :: Int, bar :: String }

data Data3
    = Data3C { foo :: Int, baz :: Bool }

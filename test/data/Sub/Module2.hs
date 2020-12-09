module Sub.Module2
    ( showInt
    , Data1(..)
    ) where

showInt :: Int -> String
showInt = show

data Data1
    = Data1Constructor1
    | Data1Constructor2 Int
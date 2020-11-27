module Foo
    ( parityString
    , printInt
    ) where

import One.Two.Some (showInt)

parityString :: Int -> String
parityString i =
    if even i then "It's even" else "It's odd"

printInt :: Int -> IO ()
printInt i = putStrLn $ showInt i

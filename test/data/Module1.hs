module Module1
    ( function1
    , function2
    ) where

import Sub.Module2 (showInt)

function1 :: Int -> String
function1 i =
    if even i then "It's even" else "It's odd"

function2 :: Int -> IO ()
function2 i = putStrLn $ showInt i

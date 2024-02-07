module Module1
    ( function1
    , function2
    ) where

import Sub.Module2 (showInt)

function1 :: Bool -> String
function1 b =
    if not b then "It's false" else "It's true"

function2 :: Int -> IO ()
function2 i = putStrLn $ showInt i

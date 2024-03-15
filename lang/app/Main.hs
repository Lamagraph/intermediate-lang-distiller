module Main (main) where

import Expression

main :: IO ()
main = print $ Lambda "x" (Lambda "y" (Application (Variable "x") (Variable "y")))
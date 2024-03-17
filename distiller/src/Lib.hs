module Lib (
    someFunc,
) where

import Syntax

f :: Expression
f = Lambda "x" (Variable "x")

someFunc :: IO ()
someFunc = print f

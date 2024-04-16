module Lib (
    someFunc,
) where

import Ast

f :: Expr
f = Lam (VarName "x") (Var $ VarName "x")

someFunc :: IO ()
someFunc = print f

module Main (main) where

import Syntax

{-
append xs ys = λ xs . λ ys . case xs of
    Nil => ys
    Cons x' xs' => Cons x' (append xs' ys)
-}

bodyOfAppend :: Expression
bodyOfAppend =
    Case
        (Variable "xs")
        [ (Pat "Nil" [], Variable "ys")
        ,
            ( Pat "Cons" ["x'", "xs'"]
            , Constructor "Cons" [Variable "x'", Application (Application (Function "append") (Variable "xs'")) (Variable "ys")]
            )
        ]

appendInExp :: FunctionHeader
appendInExp =
    Header
        "append"
        ["xs", "ys"]
        (Lambda "xs" (Lambda "ys" bodyOfAppend))

main :: IO ()
main = print appendInExp

module Main (main) where

import Ast
import Data.Map.Strict as Map
import Inference
import Text.PrettyPrint.Leijen.Text (pretty)
import TypedAst

{-
append xs ys = λ xs . λ ys . case xs of
    Nil => ys
    Cons x' xs' => Cons x' (append xs' ys)
-}

bodyOfAppend :: Expr
bodyOfAppend =
    Case
        (Var $ VarName "xs")
        $ Map.fromList
            [ (ConName "Nil", Alt [] (Var $ VarName "ys"))
            ,
                ( ConName "Cons"
                , Alt
                    [VarName "x'", VarName "xs'"]
                    ( Con (ConName "Cons") [Var $ VarName "x'", App (App (Fun $ FunName "append") (Var $ VarName "xs'")) (Var $ VarName "ys")]
                    )
                )
            ]

appendInExp :: Expr
appendInExp =
    App
        ( Fun
            (FunName "append")
        )
        (Lam (VarName "xs") (Lam (VarName "ys") bodyOfAppend))

main :: IO ()
main = do
    print appendInExp
    print $ pretty appendInExp
    case runInfer term of
        Left e -> putStrLn ("No type: " ++ show e)
        Right tyAns -> print tyAns
  where
    ty = Arrow (TypeVar 0) (TypeVar 0)
    term = Lam (VarName "x") (App (App (Var $ VarName "+") x) x)
    x = Var $ VarName "x"

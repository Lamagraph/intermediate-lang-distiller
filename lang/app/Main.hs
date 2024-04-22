module Main (main) where

import Ast
import Data.Map.Strict as Map
import Inference
import Scheme
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

one :: Expr
one = App (Fun (FunName "two")) (Con (ConName "S") [Con (ConName "Z") []])

emptyList :: Expr
emptyList = App (Fun $ FunName "emptyList") (Con (ConName "Nil") [])

listOfZero :: Expr
listOfZero = Con (ConName "Cons") [Con (ConName "Z") [], Con (ConName "Nil") []]

listOfOne :: Expr
listOfOne =
    App
        (Fun $ FunName "listOfOne")
        (Con (ConName "Cons") [Con (ConName "S") [Con (ConName "Z") []], Con (ConName "Nil") []])

kakaduEnv :: Env
kakaduEnv =
    Map.fromList
        [ ("*", arith)
        , ("/", arith)
        , ("-", arith)
        , ("+", arith)
        , ("=", eqS)
        ]
  where
    arith = Scheme.ofTy $ Arrow (Prim "int") $ Arrow (Prim "int") (Prim "int")
    eqS = Scheme.ofTy $ Arrow (TypeVar 0) $ Arrow (TypeVar 0) $ Prim "bool"

inferTest :: Expr -> IO ()
inferTest expr = do
    putStrLn ""

    print expr
    print $ pretty expr
    case runInferDefault expr of
        Left e -> putStrLn ("No type: " ++ show e)
        Right tyAns -> print tyAns

main :: IO ()
main = do
    print $ pretty term
    case runInfer kakaduEnv term of
        Left e -> putStrLn ("No type: " ++ show e)
        Right tyAns -> print tyAns

    mapM_ inferTest [one, emptyList, listOfZero, listOfOne, appendInExp]
  where
    ty = Arrow (TypeVar 0) (TypeVar 0)
    term = Lam (VarName "x") (App (App (Var $ VarName "+") x) x)
    x = Var $ VarName "x"

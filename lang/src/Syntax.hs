{-# LANGUAGE InstanceSigs #-}

module Syntax (Expression (Variable, Constructor, Lambda, Application, Case, Let, Function), Fun, Pattern (Pat), FunctionHeader (Header)) where

type Fun = String

type Var = String

data Pattern = Pat String [Var]

data Expression
    = Variable Var
    | Constructor String [Expression]
    | Lambda Var Expression
    | Application Expression Expression
    | Case Expression [(Pattern, Expression)]
    | Let Var Expression Expression
    | Function Fun

data FunctionHeader = Header Fun [Var] Expression

instance Show Expression where
    show :: Expression -> String
    show expr =
        case expr of
            Variable v -> "(" ++ show v ++ ")"
            Lambda v exp' -> "(λ" ++ show v ++ "." ++ show exp' ++ ")"
            Constructor name exps -> "(" ++ name ++ foldr (\x acc -> "(" ++ show x ++ ")" ++ acc) "" exps ++ ")"
            Application exp1 exp2 -> "(" ++ show exp1 ++ ")(" ++ show exp2 ++ ")"
            Case exp' ls -> "(case " ++ show exp' ++ "of\n" ++ foldr (\(pat, exp'') acc -> "\t" ++ show pat ++ " => " ++ show exp'' ++ "\n" ++ acc) "" ls ++ ")"
            Let var exp1 exp2 -> "let " ++ show var ++ " = " ++ show exp1 ++ " in " ++ show exp2
            Function f -> show f

instance Show Pattern where
    show :: Pattern -> String
    show (Pat patName vars) = show patName ++ " " ++ foldr (\x acc -> show x ++ " " ++ acc) "" vars

instance Show FunctionHeader where
    show :: FunctionHeader -> String
    show (Header funcName vars expr) = show funcName ++ " " ++ foldr (\x acc -> show x ++ " " ++ acc) "" vars ++ " = " ++ show expr

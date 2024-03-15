module Expression (Expression (Variable, Constructor, Lambda, Application, Case, Let, Function)) where

type Fun = String

type Var = String

newtype Pattern = String [Var] deriving (Show)

data Expression
    = Variable Var
    | Constructor String [Expression]
    | Lambda Var Expression
    | Application Expression Expression
    | Case Expression [(Pattern, Expression)]
    | Let Var Expression Expression
    | Function Fun

instance Show Expression where
    -- show :: Expression -> String
    show expr =
        case expr of
            Variable v -> "(" ++ show v ++ ")"
            Lambda v exp' -> "(λ" ++ show v ++ "." ++ show exp' ++ ")"
            Constructor name exps -> "(" ++ name ++ foldr (\x acc -> "(" ++ show x ++ ")" ++ acc) "" exps ++ ")"
            Application exp1 exp2 -> "(" ++ show exp1 ++ ")(" ++ show exp2 ++ ")"
            Case exp' ls -> "(case " ++ show exp' ++ "of\n" ++ foldr (\(pat, exp'') acc -> "\t" ++ show pat ++ " => " ++ show exp'' ++ "\n" ++ acc) "" ls ++ ")"
            Let var exp1 exp2 -> "let " ++ show var ++ " = " ++ show exp1 ++ " in " ++ show exp2
            Function f -> show f

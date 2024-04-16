module ExprTransforms where

import Ast
import Data.Map.Strict qualified as Map

multiAbstraction :: [Var] -> Expr -> Expr
multiAbstraction listOfVars expression = foldl (flip Lam) expression listOfVars

substituteExprs :: Expr -> [(Var, Expr)] -> Expr
substituteExprs = foldl substituteExpr

substituteExpr :: Expr -> (Var, Expr) -> Expr
substituteExpr expression varExp@(var, newExpression) = case expression of
    e@(Var v) ->
        if varName v == varName var
            then newExpression
            else e
    Lam v exp' -> Lam v (substituteExpr exp' varExp)
    App exp1 exp2 -> App (substituteExpr exp1 varExp) (substituteExpr exp2 varExp)
    Case caseExpr alts -> Case (substituteExpr caseExpr varExp) (Map.map (\(Alt vars exp') -> Alt vars (substituteExpr exp' varExp)) alts)
    Con con exps -> Con con (map (`substituteExpr` varExp) exps)
    e -> e

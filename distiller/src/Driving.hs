module Driving where

import Context

import Ast
import Data.Map.Strict qualified as Map
import ExprTransforms

data ProcessTree = Leaf Var | Node Expr [ProcessTree]

toProcessTree :: Expr -> EvaluationContext -> Map.Map Fun (Expr, [Var]) -> ProcessTree
toProcessTree expression context functionEnv =
    case expression of
        Var x -> Leaf x
        e@(Con con expressions) ->
            case context of
                EmptyCtx -> Node e (map (\exp' -> toProcessTree exp' context functionEnv) expressions)
                (Pair (CaseCtx alts) k) -> Node e [toProcessTree substitutedExp k functionEnv]
                  where
                    Alt vars exp' = alts Map.! con
                    substitutedExp = substituteExprs exp' (zip vars expressions)
                _ -> error "ExpCtx can not occur with Con expression"
        e@(Lam v expression') ->
            case context of
                EmptyCtx -> Node e [toProcessTree expression' context functionEnv]
                Pair (ExpCtx exp') k -> Node e [toProcessTree substitutedExp k functionEnv]
                  where
                    substitutedExp = substituteExpr expression' (v, exp')
                _ -> error "CaseCtx can not occur with Lam expression"
        e@(Fun fun) ->
            let (expression', vars) = functionEnv Map.! fun
             in Node e [toProcessTree (multiAbstraction vars expression') context functionEnv]
        e@(App e1 e2) -> Node e [toProcessTree e1 (Pair (ExpCtx e2) context) functionEnv]
        e@(Case expression' alts) -> Node e [toProcessTree expression' (Pair (CaseCtx alts) context) functionEnv]
        e -> error $ show e ++ ".  This subexpression can not occur in pure expression"

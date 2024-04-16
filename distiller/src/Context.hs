module Context where

import Ast

data ShallowReductionContext
    = ExpCtx Expr
    | CaseCtx Alts

data EvaluationContext
    = EmptyCtx
    | Pair ShallowReductionContext EvaluationContext

insertionIntoEvCtx :: EvaluationContext -> Expr -> Expr
insertionIntoEvCtx k e =
    case k of
        EmptyCtx -> e
        Pair shCtx k' ->
            case shCtx of
                ExpCtx e' -> insertionIntoEvCtx k' (App e e')
                CaseCtx alts -> insertionIntoEvCtx k' (Case e alts)

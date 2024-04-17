module Inference (Env, defaultEnv, runInfer, runInferDefault) where

import Ast
import Control.Monad.Except
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import MonadFresh
import Scheme
import Subst (Subst)
import Subst qualified as Sub
import TypedAst

instantiate :: Scheme -> MonadFresh Ty
instantiate (Scheme vars ty) = IntSet.foldr helper (return ty) vars
  where
    helper name acc = do
        newName <- freshVar
        unwrappedTy <- acc
        let sub = IntMap.singleton name newName
        return (Sub.apply sub unwrappedTy)

type Env = Map Text Scheme

lookupEnv :: Env -> Text -> MonadFresh (Subst, Ty)
lookupEnv env name = case Map.lookup name env of
    Nothing -> throwError $ NoVariable name
    Just scheme -> do
        answer <- instantiate scheme
        return (IntMap.empty, answer)

defaultEnv :: Env
defaultEnv =
    Map.fromList
        [ ("Z", Scheme.ofTy (Prim "Nat"))
        , ("S", Scheme.ofTy (Arrow (Prim "Nat") (Prim "Nat")))
        ]

-- Here probably must be Peano numbers and Lists

inferOfEither :: Either Error e -> MonadFresh e
inferOfEither = \case
    Left e -> throwError e
    Right x -> return x

unify :: Ty -> Ty -> MonadFresh Subst
unify a b = inferOfEither $ Sub.unify a b

infer :: Env -> Expr -> MonadFresh (Subst, Ty)
infer env = \case
    Var (VarName var) -> lookupEnv env var
    Con (ConName con) exprs -> do
        (conSub, conTy) <- lookupEnv env con

        return (conSub, conTy)
    Lam (VarName var) expr -> do
        typeVar <- freshVar
        let newEnv = Map.insert var (Scheme IntSet.empty typeVar) env
        (sub, ty) <- infer newEnv expr
        let resTy = Arrow (Sub.apply sub typeVar) ty
        return (sub, resTy)
    Fun (FunName fun) -> lookupEnv env fun
    App f@(Fun (FunName fun)) expr -> do
        funType <- tryError $ infer env f
        case funType of
            Left (NoVariable _) -> do
                typeVar <- freshVar
                let newEnv = Map.insert fun (Scheme IntSet.empty typeVar) env
                infer newEnv expr
            Left e -> throwError e
            Right x -> undefined -- unify types
    App lExpr rExpr -> do
        (lSub, lTy) <- infer env lExpr
        (rSub, rTy) <- infer (fmap (Scheme.apply lSub) env) rExpr
        typeVar <- freshVar
        unifiedSub <- unify (Sub.apply rSub lTy) (Arrow rTy typeVar)
        let resTy = Sub.apply unifiedSub typeVar
        final <- inferOfEither $ Sub.composeAll [lSub, rSub, unifiedSub]
        return (final, resTy)
    Case expr alts -> error "Cannot typecheck case of"
    Let var varExpr expr -> error "Cannot typecheck let"

runInfer :: Env -> Expr -> Either Error Ty
runInfer env expr = case res of
    Left err -> Left err
    Right (_, ty) -> Right ty
  where
    res = runMonadFresh (infer env expr) 0

runInferDefault :: Expr -> Either Error Ty
runInferDefault = runInfer defaultEnv

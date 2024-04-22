{-# HLINT ignore "Eta reduce" #-}
module Inference (Env, defaultEnv, runInfer, runInferDefault) where

import Ast
import Control.Monad.Except
import Data.Foldable (foldlM)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Debug.Trace
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
        [ ("Z", Scheme.ofTy $ Prim "Nat")
        , ("S", Scheme.ofTy $ Arrow (Prim "Nat") (Prim "Nat"))
        , ("Nil", Scheme.ofTy $ TyConstructor "List" $ TypeVar 0)
        ,
            ( "Cons"
            , Scheme.ofTy $
                TyConstructor "List" $
                    Arrow (TypeVar 0) $
                        Arrow (TyConstructor "List" $ TypeVar 0) (TyConstructor "List" $ TypeVar 0)
            )
        ]

inferOfEither :: Either Error e -> MonadFresh e
inferOfEither = \case
    Left e -> throwError e
    Right x -> return x

unify :: Ty -> Ty -> MonadFresh Subst
unify a b = inferOfEither $ Sub.unify a b

inferApply :: Env -> (Subst, Ty) -> Expr -> MonadFresh (Subst, Ty)
inferApply env oldType expr = do
    let (oldSub, oldTy) = oldType
    (sub, ty) <- infer (fmap (Scheme.apply oldSub) env) expr
    typeVar <- freshVar
    unifiedSub <- unify (Sub.apply sub oldTy) (Arrow ty typeVar)
    let resTy = Sub.apply unifiedSub typeVar
    final <- inferOfEither $ Sub.composeAll [oldSub, sub, unifiedSub]
    return (final, resTy)

infer :: Env -> Expr -> MonadFresh (Subst, Ty)
infer env = \case
    Var (VarName var) -> lookupEnv env var
    Con (ConName con) exprs -> do
        constType <- lookupEnv env con
        foldlM (inferApply env) constType exprs
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
            Left err -> throwError err
            Right (sub, ty) -> do
                (rSub, rTy) <- infer env expr

                error "fun app apply?"
    App lExpr rExpr -> do
        lType <- infer env lExpr
        inferApply env lType rExpr
    Case expr alts -> do
        (exprSub, exprTy) <- infer env expr
        _ <- traceShowM exprTy
        -- e <- mapM (infer env) alts
        error "case"
    Let var varExpr expr -> error "Cannot typecheck let"

runInfer :: Env -> Expr -> Either Error Ty
runInfer env expr = case res of
    Left err -> Left err
    Right (_, ty) -> Right ty
  where
    res = runMonadFresh (infer env expr) 0

runInferDefault :: Expr -> Either Error Ty
runInferDefault = runInfer defaultEnv

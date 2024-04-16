module Inference (runInfer) where

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
defaultEnv = Map.empty -- Here probably must be Peano numbers and Lists

infer :: Env -> Expr -> MonadFresh (Subst, Ty)
infer env = \case
    Var (VarName var) ->
        do
            -- env <- get

            undefined

runInfer :: Expr -> Either Error Ty
-- runInfer expr = do
--     res <- infer defaultEnv expr

--     case snd $ res 0 of
--         Left expr' -> Left expr'
--         Right (_, ty) -> Right ty
runInfer expr = undefined

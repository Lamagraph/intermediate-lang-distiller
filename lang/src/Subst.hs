{-# HLINT ignore "Eta reduce" #-}
module Subst (Subst, apply, unify, composeAll) where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Debug.Trace (traceM, traceShowM)
import MonadFresh
import TypedAst (Ty (Arrow, Prim, TyConstructor, TypeVar))
import TypedAst qualified as TA

type Subst = IntMap Ty

apply :: Subst -> Ty -> Ty
apply sub = \case
    x@(Prim _) -> x
    ty@(TypeVar x) -> fromMaybe ty (IntMap.lookup x sub)
    Arrow left right -> Arrow (apply sub left) (apply sub right)
    TyConstructor name ty -> TyConstructor name (apply sub ty)

mapping :: Int -> Ty -> Either Error (Int, Ty)
mapping key val = if TA.occursIn key val then Left OccursCheck else Right (key, val)

compose :: Subst -> Subst -> Either Error Subst
compose sub1 sub2 = IntMap.foldrWithKey extend (return sub1) sub2
  where
    extend key val acc = do
        unwrappedAcc <- acc
        case IntMap.lookup key unwrappedAcc of
            Just newVal -> do
                _ <- traceM "Entered Just in compose"
                newAcc <- unify val newVal
                compose unwrappedAcc newAcc
            Nothing -> do
                let newVal = apply unwrappedAcc val
                    newAcc = IntMap.singleton key newVal
                IntMap.foldrWithKey
                    ( \key' val' acc' -> do
                        unwrappedAcc' <- acc'
                        let newNewVar = apply newAcc val'
                        (key'', val'') <- mapping key' newNewVar
                        return $ IntMap.insert key'' val'' unwrappedAcc'
                    )
                    (return newAcc)
                    unwrappedAcc

composeAll :: [Subst] -> Either Error Subst
composeAll subs = foldr (\val acc -> acc >>= \acc' -> compose val acc') (return IntMap.empty) subs

unify :: Ty -> Ty -> Either Error Subst
unify (Prim left) (Prim right) | left == right = return IntMap.empty
unify left@(Prim _) right@(Prim _) = Left $ UnificationFailed left right
unify (TypeVar left) (TypeVar right) | left == right = return IntMap.empty
unify (TypeVar left) right = return $ IntMap.singleton left right
unify left (TypeVar right) = return $ IntMap.singleton right left
unify (Arrow lLeft lRight) (Arrow rLeft rRight) = do
    subLeft <- unify lLeft rLeft
    subRight <- unify lRight rRight
    compose subLeft subRight
unify (TyConstructor lName lTy) (TyConstructor rName rTy) | lName == rName = unify lTy rTy
unify left@(TyConstructor _ _) right@(TyConstructor _ _) = Left $ UnificationFailed left right
unify (TyConstructor _ lTy) right@(Prim _) = unify lTy right
unify (TyConstructor _ lTy) right@(Arrow _ _) = unify lTy right
unify left@(Prim _) right@(Arrow _ _) = Left $ UnificationFailed left right
unify left@(Arrow _ _) right@(Prim _) = Left $ UnificationFailed left right
unify left@(Prim _) right@(TyConstructor _ _) = Left $ UnificationFailed left right
unify left@(Arrow _ _) right@(TyConstructor _ _) = Left $ UnificationFailed left right

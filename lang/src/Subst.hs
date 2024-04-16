{-# HLINT ignore "Eta reduce" #-}
module Subst (Subst, apply) where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import MonadFresh
import TypedAst (Ty (Arrow, Prim, TypeVar))
import TypedAst qualified as TA

-- Not a newtype! Currently this is the only IntMap used
type Subst = IntMap Ty

apply :: Subst -> Ty -> Ty
apply sub = \case
    ty@(TypeVar x) -> fromMaybe ty (IntMap.lookup x sub)
    Arrow left right -> Arrow (apply sub left) (apply sub right)
    x -> x

mapping :: Int -> Ty -> Either Error (Int, Ty)
mapping key val = if TA.occursIn key val then Left OccursCheck else Right (key, val)

compose :: Subst -> Subst -> Either Error Subst
compose sub1 sub2 = IntMap.foldrWithKey extend (return sub1) sub2
  where
    extend key val acc = do
        unwrappedAcc <- acc
        case IntMap.lookup key unwrappedAcc of
            Just newVal -> do
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
composeAll subs = foldr (\val acc -> do unwrappedAcc <- acc; compose val unwrappedAcc) (return IntMap.empty) subs

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
unify left right = Left $ UnificationFailed left right

module Scheme (Scheme (Scheme), occursIn, freeVars, apply, ofTy) where

import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Subst (Subst)
import Subst qualified as Sub
import TypedAst (Ty)
import TypedAst qualified as TA

data Scheme = Scheme IntSet Ty
    deriving (Show)

occursIn :: Int -> Scheme -> Bool
occursIn var (Scheme names ty) =
    IntSet.notMember var names && TA.occursIn var ty

freeVars :: Scheme -> IntSet
freeVars (Scheme names ty) = IntSet.difference (TA.freeVars ty) names

apply :: Subst -> Scheme -> Scheme
apply sub (Scheme names ty) = Scheme names $ Sub.apply newSub ty
  where
    newSub = IntSet.foldr IntMap.delete sub names

ofTy :: Ty -> Scheme
ofTy ty = Scheme (TA.freeVars ty) ty

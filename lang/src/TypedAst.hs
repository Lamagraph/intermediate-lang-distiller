module TypedAst (Ty (..), occursIn, freeVars) where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Text (Text)

data Ty
    = Prim Text
    | TypeVar Int
    | Arrow Ty Ty
    | TyConstructor Text Ty
    deriving (Show, Eq)

occursIn :: Int -> Ty -> Bool
occursIn var = \case
    Prim _ -> False
    TypeVar x -> x == var
    Arrow left right -> occursIn var left || occursIn var right
    TyConstructor _ ty -> occursIn var ty

freeVars :: Ty -> IntSet
freeVars = helper IntSet.empty
  where
    helper acc = \case
        Prim _ -> acc
        TypeVar x -> IntSet.insert x acc
        Arrow left right -> helper (helper acc right) left
        TyConstructor _ ty -> helper acc ty

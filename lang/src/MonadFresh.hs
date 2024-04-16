module MonadFresh (Error (..), MonadFresh, runMonadFresh, fresh, freshVar) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Text (Text)
import TypedAst

data Error = OccursCheck | NoVariable Text | UnificationFailed Ty Ty
    deriving (Show)

type MonadFreshT m a = StateT Int (ExceptT Error m) a

runMonadFreshT :: (Monad m) => MonadFreshT m a -> Int -> m (Either Error a)
runMonadFreshT m = runExceptT . evalStateT m

type MonadFresh a = MonadFreshT Identity a

runMonadFresh :: MonadFresh a -> Int -> Either Error a
runMonadFresh m = runIdentity . runMonadFreshT m

fresh :: MonadFresh Int
fresh = do
    n <- get
    put $ n + 1
    return n

freshVar :: MonadFresh Ty
freshVar = fmap TypeVar fresh

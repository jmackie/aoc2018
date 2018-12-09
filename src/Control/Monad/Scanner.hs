module Control.Monad.Scanner
    ( Scanner
    , Error
    , run
    , scan
    )

where

import Prelude

import Control.Monad.Fail (MonadFail(fail))


data Error
    = NotEnoughInput
    | UserError String
    deriving Show


data Scanner s a = Scanner { run :: [s] -> Either Error (a, [s]) }


scan :: Scanner a a
scan = Scanner $ \input -> case input of
    []       -> Left NotEnoughInput
    (a : as) -> Right (a, as)


instance Functor (Scanner s) where
    fmap f scanner = Scanner $ \input -> do
        (a, input') <- run scanner input
        pure (f a, input')


instance Applicative (Scanner s) where
    pure a = Scanner $ \input -> pure (a, input)

    lhs <*> rhs = Scanner $ \input -> do
        (f, input')  <- run lhs input
        (a, input'') <- run rhs input'
        pure (f a, input'')


instance Monad (Scanner s) where
    lhs >>= rhs = Scanner $ \input -> do
        (a, input') <- run lhs input
        run (rhs a) input'


instance MonadFail (Scanner s) where
    fail err = Scanner $ \_ -> Left (UserError err)

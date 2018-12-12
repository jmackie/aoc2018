{-# OPTIONS -fno-warn-incomplete-patterns #-}
module Data.Universe
    ( Universe(Universe)
    , left
    , right
    , view
    )
where

import Prelude

import Control.Comonad


data Universe a = Universe [a] a [a]


instance Functor Universe where
    fmap f (Universe a b c) =
        Universe (fmap f a) (f b) (fmap f c)


instance Comonad Universe where
    duplicate a = Universe (tail $ iterate left a) a (tail $ iterate right a)
    extract (Universe _ b _) = b
    -- NOTE: duplicate == cojoin
    --       extend    == cobind
    --       extract   == coreturn


left :: Universe a -> Universe a
left (Universe (a : as) b c) = Universe as a (b : c)


right :: Universe a -> Universe a
right (Universe a b (c : cs)) = Universe (b : a) c cs


view :: Int -> Universe a -> [a]
view n (Universe a b c) = reverse (take n a) <> [b] <> take n c

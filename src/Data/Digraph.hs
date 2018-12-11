{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Digraph
    ( Digraph(Digraph, vertices, edges)
    , Arrow((:->))
    , removeEdge
    , tsort
    , tsortWith
    , earliest
    , disucc
    , dipred
    , head
    , tail
    )
where

import Prelude hiding (head, tail)

import qualified Data.List as List
import qualified Data.Set as Set

import Data.Set (Set)


-- | Directed graph.
data Digraph a = Digraph { vertices :: Set a, edges :: Set (Arrow a) }


-- | Directed graph edge.
data Arrow a = a :-> a deriving (Show, Eq, Ord)


-- | Remove an edge from a directed graph.
removeEdge :: Ord a => Arrow a -> Digraph a -> Digraph a
removeEdge edge digraph = digraph { edges = Set.delete edge (edges digraph) }

--
-- | Topologically sort a directed graph.
--
-- Returns 'Nothing' if the graph is cyclic.
tsort :: Ord a => Digraph a -> Maybe [a]
tsort = tsortWith (\_ _ -> LT)


-- | Topologically sort a directed graph with a custom comparison function.
--
-- Returns 'Nothing' if the graph is cyclic.
tsortWith :: forall a . Ord a => (a -> a -> Ordering) -> Digraph a -> Maybe [a]
tsortWith cmp digraph = go
    []
    (List.sortBy cmp . Set.toList $ earliest digraph)
    digraph
  where
    go :: [a] -> [a] -> Digraph a -> Maybe [a]
    go accum [] dg | null (edges dg) = Just (reverse accum)
                   | otherwise       = Nothing
    go accum (v : vs) dg =
        let vsucc  = disucc v dg
            vedges = Set.map (v :->) vsucc
            dg'    = Set.foldr removeEdge dg vedges
            vs'    = filter (nopred dg') (Set.toList vsucc)
        in  go (v : accum) (List.sortBy cmp (vs <> vs')) dg'

    nopred :: Digraph a -> a -> Bool
    nopred dg a = null (dipred a dg)


-- | Earliest edges in a directed graph.
earliest :: Ord a => Digraph a -> Set a
earliest Digraph { vertices, edges } =
    Set.filter (`notElem` Set.map head edges) vertices


-- | Direct successor of a vertex.
disucc :: Ord a => a -> Digraph a -> Set a
disucc a = Set.map head . Set.filter ((== a) . tail) . edges


-- | Direct predecessor of a vertex.
dipred :: Ord a => a -> Digraph a -> Set a
dipred a = Set.map tail . Set.filter ((== a) . head) . edges


-- | Head of an edge.
head :: Arrow a -> a
head (_ :-> y) = y


-- | Tail of an edge.
tail :: Arrow a -> a
tail (x :-> _) = x

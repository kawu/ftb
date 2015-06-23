{-# LANGUAGE RecordWildCards #-}


-- Generic `Tree` type with different leaf and internal values.


module NLP.FTB.Tree
(
-- * Tree
  Tree (..)
, showTree
, showTree'
, toWord
) where


import           Control.Applicative ((<$>))
import           Control.Arrow (first)
import           Control.Monad (foldM)


-- | A tree with values of type 'a' kept in the interior nodes,
-- and values of type 'b' kept in the leaf nodes.
data Tree a b
    = INode -- ^ Interior node
        { labelI    :: a
        , subTrees  :: [Tree a b] }
    | FNode -- ^ Frontier node
        { labelF    :: b }
    deriving (Show, Eq, Ord)


-- | List of frontier values. 
toWord :: Tree a b -> [b]
toWord t = case t of
    INode{..}   -> concatMap toWord subTrees
    FNode{..}   -> [labelF]


-- | Show a tree given the showing functions for label values.
showTree :: (a -> String) -> (b -> String) -> Tree a b -> String
showTree f g = unlines . go
  where
    go t = case t of
        INode{..}   -> ("INode " ++ f labelI)
            : map ("  " ++) (concatMap go subTrees)
        FNode{..}   -> ["FNode " ++ g labelF]


-- | Like `showTree`, but using the default `Show` instances
-- to present label values.
showTree' :: (Show a, Show b) => Tree a b -> String
showTree' = showTree show show

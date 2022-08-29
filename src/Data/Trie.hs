module Data.Trie
    ( Trie
    , singleton, fromList
    , terminal, step
    ) where

import Control.Applicative

import qualified Data.Map.Strict as M

data Trie k v
    = TrieNode !(Maybe v) !(M.Map k (Trie k v))

instance Ord k => Monoid (Trie k v) where
    mempty = TrieNode Nothing M.empty

instance Ord k => Semigroup (Trie k v) where
    TrieNode v0 ys0 <> TrieNode v1 ys1 =
        TrieNode (v1 <|> v0) (M.unionWith (<>) ys0 ys1)

singleton :: Ord k => [k] -> v -> Trie k v
singleton = go
  where
    go [] v = TrieNode (Just v) M.empty
    go (x:xs) v = TrieNode Nothing (M.singleton x (go xs v))

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldMap (uncurry singleton)

terminal :: Trie k v -> Maybe v
terminal (TrieNode v _) = v

step :: Ord k => k -> Trie k v -> Trie k v
step k (TrieNode _ xs) = M.findWithDefault mempty k xs

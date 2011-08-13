module Trie where

type Trie = [TrieEdge]

data TrieEdge = TrieEdge Char Bool Trie deriving Show

toList :: Trie -> [String]
toList = concatMap toList'
    where toList' (TrieEdge c w t) = let rest = (map (c:) (toList t))
                                     in if w then [c]:rest else rest

fromList :: [String] -> Trie
fromList = foldr insert []

insert :: String -> Trie -> Trie
insert [] t = t
insert w [] = fromWord w
insert w@(c:[]) ((e@(TrieEdge ec ew et)):ts) = if c == ec
                                         then ((TrieEdge ec True et):ts)
					 else e:(insert w ts)
insert w@(c:cs) ((e@(TrieEdge ec ew et)):ts) = if c == ec
                                               then (TrieEdge ec ew (insert cs et)):ts
					       else e:(insert w ts)

fromWord :: String -> Trie
fromWord [] = []
fromWord (c:[]) = [TrieEdge c True []]
fromWord (c:cs) = [TrieEdge c False (fromWord cs)]

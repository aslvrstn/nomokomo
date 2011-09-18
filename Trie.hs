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
                                               then (TrieEdge ec True et):ts
                                               else e:(insert w ts)
insert w@(c:cs) ((e@(TrieEdge ec ew et)):ts) = if c == ec
                                               then (TrieEdge ec ew (insert cs et)):ts
                                               else e:(insert w ts)

fromWord :: String -> Trie
fromWord [] = []
fromWord (c:[]) = [TrieEdge c True []]
fromWord (c:cs) = [TrieEdge c False (fromWord cs)]

member :: String -> Trie -> Bool
member [] _ = False
member _ [] = False
member w@(c:[]) ((TrieEdge ec ew et):ts) = if c == ec
                                           then ew
                                           else member w ts
member w@(c:cs) ((TrieEdge ec ew et):ts) = if c == ec
                                           then member cs et
                                           else member w ts

matching :: Trie -> [String] -> [String]
matching t ss = map reverse $ matching' t ss []
                where matching' _ [] _ = []
                      matching' [] _ _ = []
                      matching' t ([]:ss) pw = matching' t ss pw
                      matching' ((TrieEdge ec ew et):ts) p@((c:cs):ss) pw = if c == ec
                                                                            then (if ew then (c:pw):(matching' et ss (c:pw)) else (matching' et ss (c:pw))) ++ (matching' ts (cs:ss) pw)
                                                                            else matching' ts p pw

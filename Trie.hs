module Trie where

import qualified Data.Map

type Trie = Data.Map.Map Char TrieEdge

data TrieEdge = TrieEdge Bool Trie deriving (Show, Eq)

toList :: Trie -> [String]
toList = Data.Map.foldWithKey (\c (TrieEdge w t) r -> let rest = (map (c:) (toList t))
                                                      in r ++ (if w then [c]:rest else rest)) []

empty :: Trie
empty = Data.Map.empty

fromList :: [String] -> Trie
fromList = foldr insert Data.Map.empty

insert :: String -> Trie -> Trie
insert [] = id
insert (c:cs) = Data.Map.alter (\e -> Just (maybe (TrieEdge (cs == []) (fromWord cs)) (\(TrieEdge ew et) -> TrieEdge (cs == [] || ew) (insert cs et)) e)) c

fromWord :: String -> Trie
fromWord = foldr (\c t -> Data.Map.fromList [(c, TrieEdge (t == Data.Map.empty) t)]) Data.Map.empty

member :: String -> Trie -> Bool
member [] t = False
member (c:cs) t = maybe False (\(TrieEdge ew et) -> (cs == [] && ew) || member cs et) $ Data.Map.lookup c t

-- matching dict ["at", "r", "ckt", "s"] -> fromList ["a", "arc", "ark", "art", "arcs", "arks", "arts"]
matching :: Trie -> [String] -> Trie
matching t [] = Data.Map.empty
matching t (s:ss) = Data.Map.mapMaybeWithKey (\k (TrieEdge ew et) -> if elem k s then Just (TrieEdge ew (matching et ss)) else Nothing) t

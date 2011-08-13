data Trie = Trie [TrieEdge] deriving Show

data TrieEdge = TrieEdge Char Bool Trie deriving Show

empty :: Trie
empty = Trie []

toList :: Trie -> [String]
toList (Trie []) = []
toList (Trie ((TrieEdge c w t):es)) = let rest = (map (c:) (toList t))
                                      in if w then [c]:rest else rest

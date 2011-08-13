type Trie = [TrieEdge]

data TrieEdge = TrieEdge Char Bool Trie deriving Show

toList :: Trie -> [String]
toList [] = []
toList ((TrieEdge c w t):es) = let rest = (map (c:) (toList t))
                               in if w then [c]:rest else rest

module Trie where

import qualified Data.Map

type GaddagChar = Maybe Char

type Gaddag = Data.Map.Map GaddagChar GaddagArc

data GaddagArc = GaddagArc Bool Gaddag deriving (Show, Eq)

toList :: Gaddag -> [[GaddagChar]]
toList = Data.Map.foldWithKey (\c (GaddagArc w t) r -> let rest = (map (c:) (toList t))
                                                      in r ++ (if w then [c]:rest else rest)) []

pretty :: [GaddagChar] -> String
pretty = map (maybe '^' id)

empty :: Gaddag
empty = Data.Map.empty

fromList :: [String] -> Gaddag
fromList = foldr insert empty

insert :: String -> Gaddag -> Gaddag
insert [] = id
insert w = createPathA w

createPathA :: String -> Gaddag -> Gaddag
createPathA w = createPathA' (reverse w)
                where createPathA' [] = id
                      createPathA' (c:cs) = Data.Map.alter (\e -> Just (maybe (GaddagArc (cs == []) (createPathA' cs empty)) (\(GaddagArc ew et) -> GaddagArc (cs == [] || ew) (insert cs et)) e)) (Just c)

--fromWord :: String -> Trie
--fromWord = foldr (\c t -> Data.Map.fromList [(c, TrieEdge (t == empty) t)]) empty

--member :: String -> Trie -> Bool
--member [] t = False
--member (c:cs) t = maybe False (\(TrieEdge ew et) -> (cs == [] && ew) || member cs et) $ Data.Map.lookup c t

-- matching dict ["at", "r", "ckt", "s"] -> fromList ["a", "arc", "ark", "art", "arcs", "arks", "arts"]
--matching :: Trie -> [String] -> Trie
--matching t [] = empty
--matching t (s:ss) = Data.Map.mapMaybeWithKey (\k (TrieEdge ew et) -> if elem k s then Just (TrieEdge ew (matching et ss)) else Nothing) t

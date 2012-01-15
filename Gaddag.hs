module Trie where

import qualified Data.List
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
insert w = insertGC (map Just w)

delimit :: String -> [[GaddagChar]]
delimit w = let gw = map Just w
            in tail $ zipWith (\a b -> reverse a ++ [Nothing] ++ b) (Data.List.inits gw) (Data.List.tails gw)

insertGC :: [GaddagChar] -> Gaddag -> Gaddag
insertGC [] = id
insertGC (c:cs) = Data.Map.alter (\e -> Just (maybe (GaddagArc (cs == []) (insertGC cs empty)) (\(GaddagArc ew et) -> GaddagArc (cs == [] || ew) (insertGC cs et)) e)) c

--fromWord :: String -> Trie
--fromWord = foldr (\c t -> Data.Map.fromList [(c, TrieEdge (t == empty) t)]) empty

--member :: String -> Trie -> Bool
--member [] t = False
--member (c:cs) t = maybe False (\(TrieEdge ew et) -> (cs == [] && ew) || member cs et) $ Data.Map.lookup c t

-- matching dict ["at", "r", "ckt", "s"] -> fromList ["a", "arc", "ark", "art", "arcs", "arks", "arts"]
--matching :: Trie -> [String] -> Trie
--matching t [] = empty
--matching t (s:ss) = Data.Map.mapMaybeWithKey (\k (TrieEdge ew et) -> if elem k s then Just (TrieEdge ew (matching et ss)) else Nothing) t

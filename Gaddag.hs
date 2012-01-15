module Gaddag where

import qualified Data.List
import qualified Data.Map

type GaddagChar = Maybe Char

type GaddagWord = [GaddagChar]

type Gaddag = Data.Map.Map GaddagChar GaddagArc

data GaddagArc = GaddagArc Bool Gaddag deriving (Show, Eq)

toList :: Gaddag -> [GaddagWord]
toList = Data.Map.foldWithKey (\c (GaddagArc w t) r -> let rest = (map (c:) (toList t))
                                                      in r ++ (if w then [c]:rest else rest)) []
toString :: GaddagWord -> String
toString = map (maybe '^' id)

gFromString :: String -> GaddagWord
gFromString = map Just

empty :: Gaddag
empty = Data.Map.empty

fromList :: [String] -> Gaddag
fromList = foldr insert empty

insert :: String -> Gaddag -> Gaddag
insert [] g = g
insert w g = foldr insertGC g (delimit w)

delimit :: String -> [GaddagWord]
delimit w = let gw = map Just w
            in tail $ zipWith (\a b -> reverse a ++ [Nothing] ++ b) (Data.List.inits gw) (Data.List.tails gw)

insertGC :: GaddagWord -> Gaddag -> Gaddag
insertGC [] = id
insertGC (c:cs) = Data.Map.alter (\e -> Just (maybe (GaddagArc (cs == []) (insertGC cs empty)) (\(GaddagArc ew et) -> GaddagArc (cs == [] || ew) (insertGC cs et)) e)) c

member :: GaddagWord -> Gaddag -> Bool
member [] t = False
member (c:cs) t = maybe False (\(GaddagArc ew et) -> (cs == [] && ew) || member cs et) $ Data.Map.lookup c t

-- matching dict ["at", "r", "ckt", "s"] -> fromList ["a", "arc", "ark", "art", "arcs", "arks", "arts"]
--matching :: Trie -> [String] -> Trie
--matching t [] = empty
--matching t (s:ss) = Data.Map.mapMaybeWithKey (\k (TrieEdge ew et) -> if elem k s then Just (TrieEdge ew (matching et ss)) else Nothing) t

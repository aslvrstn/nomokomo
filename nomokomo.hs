import System
import Data.List
import Data.Maybe

import Gaddag

type Board = [[Maybe Char]]

-- TODO: Potential is maybe a Monad?
data Potential = Filled Char | Empty [Char] deriving Show

--rwords :: Trie -> [Bool] -> [Potential] -> [Trie]
--rwords dict [] [] = []
--rwords dict (a:at) ps@(p:pt) = (if a then pwords dict ps else empty) : rwords dict at pt
--                               where pwords dict ps = matching dict $ map unp ps

--words :: Gaddag -> [Bool] -> [Potential] -> [Gaddag]
--words dict [] [] = []
--words dict (a:at) ps@(p:pt) = (if a then pwords dict ps else empty) : words dict a pt
--                              where pwords dict ps = matching dict $ map unp ps

unp :: Potential -> String
unp (Filled c) = [c]
unp (Empty s) = s

banchors :: Board -> [[Bool]]
banchors board = zipWith (zipWith (||)) (map anchors board) (transpose $ map anchors (transpose board))

anchors :: [Maybe Char] -> [Bool]
anchors row = let sRow = Nothing:row++[Nothing]
              in map (\i -> isNothing (sRow!!i) && (isJust (sRow!!(i-1)) || isJust (sRow!!(i+1)))) [1..length row]

crossChecks :: Gaddag -> [Maybe Char] -> [Potential]
crossChecks dict row = map checks [0..length row-1]
                       where checks i = case (row!!i) of Just l -> Filled l
                                                         Nothing -> Empty (filter (\c -> let w = gFromString (c : leftAdj i row) ++ [Nothing] ++ (gFromString (rightAdj i row))
                                                                                         in length w == 2 || member w dict) ['a'..'z'])

leftAdj i row = takeWhileJust . drop (length row-i) . reverse $ row
rightAdj i row = takeWhileJust . drop (i+1) $ row

takeWhileJust = catMaybes . takeWhile isJust

fromString :: [String] -> Board
fromString s = map (map (\c -> if c == ' ' then Nothing else Just c)) s

main = do
         (dictFile:_) <- getArgs
         dictf <- readFile dictFile
         let dict = fromList.lines $ dictf
         let board = fromString ["  ell  "]
--         putStrLn $ show (map (\r -> rwords dict (anchors r) (crossChecks dict r)) board)
         print 0

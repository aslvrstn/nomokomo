import System
import Data.List
import Data.Maybe

import Trie

type Board = [[Maybe Char]]

data Potential = Filled Char | Empty [Char] deriving Show

banchors :: Board -> [[Bool]]
banchors board = zipWith (zipWith (||)) (map anchors board) (transpose $ map anchors (transpose board))

anchors :: [Maybe Char] -> [Bool]
anchors row = let sRow = Nothing:row++[Nothing]
              in map (\i -> isNothing (sRow!!i) && (isJust (sRow!!(i-1)) || isJust (sRow!!(i+1)))) [1..length row]

crossChecks :: Trie -> [Maybe Char] -> [Potential]
crossChecks dict row = map checks [0..length row-1]
                       where checks i = case (row!!i) of Just l -> Filled l
                                                         Nothing -> Empty (filter (\c -> let w = leftAdj i row ++ [c] ++ rightAdj i row
                                                                                         in member w dict || w == [c]) ['a'..'z'])

leftAdj i row = reverse . takeWhileJust . drop (length row-i) . reverse $ row
rightAdj i row = takeWhileJust (drop (i+1) row)

takeWhileJust = catMaybes . takeWhile isJust

fromString :: [String] -> Board
fromString s = map (map (\c -> if c == ' ' then Nothing else Just c)) s

main = do
         (dictFile:_) <- getArgs
         dict <- readFile dictFile
         putStrLn $ show . fromList.lines $ dict

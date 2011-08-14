import System
import Data.List
import Data.Maybe

import Trie

data Potential = Filled Char | Empty [Char] deriving Show

crossChecks :: Trie -> [Maybe Char] -> [Potential]
crossChecks dict row = map checks [0..length row-1]
                       where checks i = case (row!!i) of Just l -> Filled l
		                                         Nothing -> Empty (filter (\c -> member (leftAdj i row ++ [c] ++ rightAdj i row) dict) ['a'..'z'])

leftAdj i row = reverse $ takeWhileJust (drop (length row-i) (reverse row))
rightAdj i row = takeWhileJust (drop (i+1) row)

takeWhileJust = catMaybes . (takeWhile isJust)

main = do
         (dictFile:_) <- getArgs
         dict <- readFile dictFile
	 putStrLn $ show ((fromList.lines) dict)

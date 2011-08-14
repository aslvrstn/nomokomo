import System

import Trie

main = do
         (dictFile:_) <- getArgs
         dict <- readFile dictFile
	 putStrLn $ show ((fromList.lines) dict)

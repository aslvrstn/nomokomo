import Trie

import Data.List
import Test.QuickCheck

main = quickCheck (\ws -> ((nub.sort) $ (delete "" ws)) == ((toList.fromList) ws))

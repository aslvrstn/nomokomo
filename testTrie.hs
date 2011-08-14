import Trie

import Data.List
import Test.QuickCheck

main = quickCheck (\ws -> delete "" ((nub.sort) ws) == (sort $ (toList.fromList) ws))

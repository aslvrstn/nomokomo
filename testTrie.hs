import Trie

import Data.List
import Test.QuickCheck

args = Args Nothing 200 500 20 True
gen = listOf . listOf1 $ elements ['a'..'z']

main = verboseCheckWith args (forAll gen (\ws -> (nub.sort) ws == (sort $ (toList.fromList) ws)))

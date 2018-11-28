module DataSet where

import Data.Set

text = "edcbaedcba"
myset1 = fromList text

l1 = [1, 2, 3, 4]
l2 = [3, 4, 5, 6]
myset2 = intersection (fromList l1) (fromList l2)
myset3 = difference (fromList l1) (fromList l2)

module BinaryTree where

data BinaryTree = Nil | Node Int BinaryTree BinaryTree

tree :: BinaryTree
tree = (Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) Nil))

inOrder :: BinaryTree -> [Int]
inOrder Nil = []
inOrder (Node v lt rt) = inOrder lt ++ v : inOrder rt

preOrder :: BinaryTree -> [Int]
preOrder Nil = []
preOrder (Node v lt rt) = v : preOrder lt ++ preOrder rt

postOrder :: BinaryTree -> [Int]
postOrder Nil = []
postOrder (Node v lt rt) = postOrder lt ++ postOrder rt ++ [v]

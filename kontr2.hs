data BST a = ET | Node a (BST a) (BST a) deriving (Show)

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a 
bstinsert x ET = Node x ET ET
bstinsert x (Node r lt rt)
	| x < r = Node r (bstinsert x lt) rt
	| otherwise = Node r lt $ bstinsert x rt

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstvalues :: BST a -> [a]              
bstsize :: BST a -> Integer  
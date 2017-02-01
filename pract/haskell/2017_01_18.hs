data Tree a = ET | Node a (Tree a) (Tree a) deriving Show

type IntTree = Tree Int

tree::IntTree
tree = (Node 10 
	(Node 7 
		(Node 4 ET (Node 6 ET ET))
		ET)
	(Node 20
		(Node 18
			(Node 16 ET ET) ET)
		(Node 40
			(Node 30 ET ET)
			(Node 50 ET ET))))

height:: Tree a -> Int
height ET = 0
height (Node r lt rt) = 1 + max (height lt) (height rt)

elem1:: (Eq a)=> a-> Tree a -> Bool
elem1 _ ET = False
elem1 x (Node r lt rt)
	| x == r = True
	| otherwise = elem1 x lt || elem1 x rt

mapTree::(a->b) ->Tree a -> Tree b
mapTree _ ET = ET
mapTree f (Node r lt rt) = Node (f r) (mapTree f lt) (mapTree f rt)

level::Int -> Tree a -> [a]
level _ ET = []
level 0 (Node r lt rt) = [r]
level x (Node r lt rt) = level (x - 1) lt ++ level (x - 1) rt

dfs::Tree a -> [a]
dfs ET = []
dfs (Node r lt rt) = [r] ++ dfs lt ++ dfs rt

 
















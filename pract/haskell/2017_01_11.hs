data Boolean = Falsy | Truthy deriving (Show, Eq, Ord)

isEven::(Integral a) => a -> Boolean
isEven x = if x `mod` 2 == 0 then Truthy else Falsy

data FailingDouble = Failure | Success Double deriving(Show)
divide:: Double -> Double -> FailingDouble
divide _ 0 = Failure
divide a b = Success $ a / b

data Day = Monday | Tuesday | Friday | Saturday | Sunday 
	deriving(Show, Eq, Ord, Enum, Bounded)
	
isWeekend:: Day -> Boolean
isWeekend Saturday = Truthy
isWeekend Sunday = Truthy
isWeekend _ = Falsy

--връщат Bool, а не Boolean, ама кот такоа
--isWeekend day = elem day [Saturday, Sunday]
--isWeekend = (>Friday)

--data Student = Person String String Int deriving(Show)
data Student = Person { fn::String,
						name::String,
						exams::Int} deriving (Show)

s = Person "12345" "John Doe" 7

--getExamsCount:: Strudent -> Int
--getExamsCount(Person _ _ e) = e

reject:: [Student] -> [Student]
reject = filter (\(Person _ _ x)-> x > 3)
-- reject = filter $ (>3).getExamsCount

{-data Tree = 
	ET | Node {root::Int, left::Tree, right::Tree} 
	deriving(Show)
-}
data Tree a = ET | Node a (Tree a) (Tree a)	

root:: Tree a -> Maybe a
root ET = Nothing
root (Node x _ _ )= Just x

left::Tree a -> Tree a
left (Node _ x _) = x

right::Tree a -> Tree a
right (Node _ _ x) = x

tree = (Node 10
	(Node 5 ET (Node 8 ET ET))
	(Node 20
		(Node 17 (Node 15 ET ET) ET)
		(Node 30
			(Node 25 ET ET)
			(Node 40 ET ET))))

isLeaf:: Tree a -> Bool
isLeaf (Node _ ET ET) = True
isLeaf _ = False

inOrder:: Tree a -> [a]
inOrder ET = []
inOrder(Node r lt rt) = inOrder lt ++ [r] ++ inOrder rt

insertTree:: (Ord a ) => a -> Tree a -> Tree a
insertTree x ET = Node x ET ET
insertTree x (Node r lt rt)
	| x < r = Node r (insertTree x lt) rt
	| otherwise = Node r lt (insertTree x rt)

foldTree f nv ET = nv
foldTree f nv t = foldr nv f inOrder t






length' l = sum [1 | _ <- l]

reverse_digits n = helper 0 n where
	helper res curr
		| curr == 0 = res
		| otherwise = helper (res * 10 + mod curr 10)
							 (div curr 10)

list_digits n = helper [] n where
	helper res curr
		| curr< 10 = (curr:res)
		| otherwise = helper ((mod curr 10):res) (div curr 10)

elem1 x l 
	| null l = False
	| head l == x = True
	| otherwise = elem1 x (tail l)

elem2 _ [] = False
elem2 x (y:ys) = x == y || elem2 x ys

elem3 x = foldr (\el r -> r || el == x) False

map1 p l = [ p x | x <- l]

filter1 f l = [ x | x <- l, f x]

map2 _ [] = []
map2 f (x:xs) = f x : map f xs

filter2 _ [] = []
filter2 f (x:xs) = if f x then
					x: filter2 f xs else
					filter f xs

intersection l1 l2 = [ x | x <- l1 , elem1 x l2 ]

union l1 l2 = l1 ++ [ x | x<- l2 , not (elem x l1)]

f = filter odd . map (\ x-> x + 1)
l = [1,2,7,-4,19,-52]

negate' x = -x

neg = map (negate' . abs)

laino1 :: Ord a => [a] -> a
laino1 = foldr1 (\x y -> if x < y then y else x)

laino2 :: Ord a => [a] -> a
laino2 = foldr1 max













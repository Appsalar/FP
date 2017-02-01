--takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
	| p x = x : takeWhile' p xs
	| otherwise = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

zipWith'' f lst1 lst2 
	| null lst1 || null lst2 = []
	| otherwise = (f (head lst1) (head lst2)) : 
	zipWith'' f (tail lst1) (tail lst2)
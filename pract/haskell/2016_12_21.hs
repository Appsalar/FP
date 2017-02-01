import Data.List (sortBy)

remove_non_uppercase s = [x | x <- s, elem x ['A'..'Z'] ]

prime n = [x | x<- [1..n], n `mod` x == 0] == [1,n]

-- няма повторение
--prime_factorization  n = [x | x <- [2..n], prime x, n `mod` x == 0]

{-prime_factorization n
	| prime n = [n]
	| otherwise = p: prime_factorization(n `div` p)
		where get-prime number = head [x | x <- [1..number], ]
		-}

zip' :: [a]->[b]->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

palindrome l = foldr (\(f,l) acc -> acc && f == l) True pairs
	where pairs = zip l $ reverse l

shan0 :: String -> String
shan0 sentence = 
	unwords 
	$ map fst 
	$ sortBy (\ p1 p2 -> compare (snd p1)(snd p2)) 
	$ map (\w -> (w, get_position w)) 
	$ words sentence
		where get_position str = head [x | x<- str, elem x ['1'..'9'] ]


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let 
		smaller = quicksort [s | s<-xs, s<=x]
		bigger = quicksort [b | b <- xs, b>x]
	in smaller ++ [x] ++ bigger

compress [] = []
compress [x] = [x]
compress (x:xs)
	| x == head xs = compress xs
	| otherwise = x : compress xs


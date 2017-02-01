sumProducts :: Num a => [[a]] -> a
sumProducts ll = sum (map product ll)

occurrences lst1 lst2 = map (\ x -> count x lst2) lst1
	where count el lst = length (filter (\ x -> x == el) lst)

transpose :: [[a]] -> [[a]]
transpose m
	| null ( head m) = []
	| otherwise = firstCol m : transpose (removeFirstCol m)
	where firstCol m = map head m
			removeFirstCol m = map tail m
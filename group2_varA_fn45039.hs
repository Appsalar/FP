import Data.List

distSq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

findDiff x lst = (x, [distSq x y | y <- lst])

findMedoid lst = fst $ minimumBy (\ (_,x) (_,y) -> 
	if x < y then LT else GT) $ 
	map (\ (a, lst)-> (a, sum lst)) [findDiff x lst | x<- lst]


points = [(2,8),(-2,4),(1,2),(-4,-1),(5,0)]

{-
sumLast k n = [genNext x k n | x <- [1..]]

генерирам всеки елемент като обърна списъка с вече съществуващите,
взимам n елемента,сумирам ги и конкатенирам резултата с останалите,
ама гърми LEL
helper n to now res = if now == to 
	then res ++ [(sum $ take n $ reverse res)]
	else helper n to (now + 1) $ res ++ (sum $take n $ reverse res)
genNext 1 k _ = k
genNext i k n = head $ reverse $ helper n 2 [k]

-}
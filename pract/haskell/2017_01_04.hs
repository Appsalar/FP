import Data.List   --включва модул; взимаме тоя за sortBy

import qualified Data.Char as C (isDigit, chr, ord)
--разграничава неймспейса 

import qualified Data.Map as Map 

convert str = filter C.isDigit str

sumNumInStr str = sum $ map read $ map convert str

student = [
	("1234", ("tommy",4.25)),
	("1654", ("nigger",4.45)),
	("2354", ("Donger",5.85))]

findKey:: (Eq k)=>k->[(k,v)]->v
findKey key = snd.head.filter (\(k, v)->key==k)

keys = map fst
-- keys [] = []
-- keys ((k,v):xs) = k : keys xs

values = map snd

findKey2:: (Eq k)=> k -> [(k,v)] -> Maybe v
findKey2 _ [] = Nothing
findKey2 key ((k,v): xs) = if key == k then Just v
	else findKey2 key xs

--avg:: Num a => (b,(c,a)) -> a
avg lst = sum grades / 
	(fromIntegral.length $ lst)
	where grades = map snd $ values lst

studentsMap = Map.fromList student

scholarship st= Map.elems . Map.map fst . Map.filter(\
	(_, grade) -> grade >= 5.5) $ st
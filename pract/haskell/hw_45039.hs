import qualified Data.Map as Map
import qualified Data.List as List

duplicateElements:: [a] -> [a]
duplicateElements lst 
	| null lst = []
	| otherwise = [head lst] ++ [head lst] ++ duplicateElements (tail lst)

slice:: Int -> Int -> [a] -> [a]
slice i j lst = take (j - i + 1) $ drop i lst 

primeSum:: Integral a => a -> (a, a)
primeSum n = (head $ dels n, n - (head $ dels n)) where 
	prime n = [x | x<- [1..n], n `mod` x == 0] == [1,n]
	dels n = [x | x <- [2..n], prime x, prime (n - x)]

type PhoneBook = Map.Map String [String]

ukazatel = [
	("az", ["laina"]),
	("negur", ["brakma"]),
	("az", ["hehe"]),
	("az", ["laina"])]

createPhoneBook:: [(String, [String])] -> PhoneBook
createPhoneBook numbers = Map.fromList 
	[(x, List.nub $ phones x numbers) | x<-names] where 
	names = List.nub [ fst a | a<-numbers]
	phones _ [] = []
	phones name (x:xs)
		| fst x == name = snd x ++ phones name xs
		| otherwise = phones name xs
	

addContact:: String -> String -> PhoneBook -> PhoneBook
addContact name number phoneBook = Map.insertWith List.union 
	name [number] phoneBook

getNumber:: String -> PhoneBook -> Maybe [String]
getNumber name phoneBook = Map.lookup name phoneBook
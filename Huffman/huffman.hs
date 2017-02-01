import qualified Data.Map as Map 
import qualified Data.Maybe as Mb
import qualified Data.List as List

foo::Map.Map Char Int
foo = Map.fromList []

insertFreqLst:: (a -> a -> Bool)-> a -> [(a, Int)] -> [(a, Int)]
insertFreqLst _ el [] = [(el,1)]
insertFreqLst f el (x:xs)
	| f el $ fst x = (fst x, snd x + 1):xs
	| otherwise = x:insertFreqLst f el xs

freqCnt::(a -> a -> Bool)-> [a] -> [(a, Int)] -> [(a, Int)]
freqCnt _ [] cont = cont
freqCnt f (x:xs) cont = freqCnt f xs $ insertFreqLst f x cont

data Tree a = ET | Leaf a Int | Node Int (Tree a) (Tree a) deriving Show

t::Tree Char
t = (Node 12
	(Leaf 'a' 5)
	(Node 123
		(Leaf 'b' 1231)
		(Node 123
			(Leaf 'c' 12)
			(Leaf 'd' 123))))

getFreq::Tree a -> Int
getFreq ET = 0
getFreq (Leaf _ x) = x
getFreq (Node x _ _) = x

instance Eq (Tree x) where
	a == b = getFreq a == getFreq b
	
instance Ord (Tree x) where
	compare a b = compare (getFreq a) (getFreq b)

genLeafs cont = map (\ (key, freq)-> (Leaf key freq)) cont

createHuffmanTree:: [Tree a] -> Tree a
createHuffmanTree [] = ET
createHuffmanTree lst = head $ helper $ List.sort lst where
	createNode l r = (Node (getFreq l + getFreq r) r l)
	helper (x:xs)
		| null xs = [x]
		| otherwise = helper $ List.sort ((createNode x $ head xs) : tail xs)

genHuffmanCode:: Tree a -> [(a, String)]
genHuffmanCode (Node _ lt rt) = 
	(genSingleCode lt "0") ++ (genSingleCode rt "1") where
	genSingleCode (Node _ lt rt) code = 
		(genSingleCode lt (code ++ "0") ) ++ (genSingleCode rt (code ++ "1"))
	genSingleCode (Leaf x _) code = [(x, code)]

genCompressed::  (a -> a -> Bool)-> [a] -> [(a, String)] -> String
genCompressed _ [] _ = []
genCompressed f (x:xs) cont = lookupDummyMap f x cont ++ genCompressed f xs cont where
	lookupDummyMap _ _ [] = []
	lookupDummyMap f el (x:xs)
		| f el $ fst x = snd x
		| otherwise = lookupDummyMap f el xs

compress f str = genCompressed f str
	$ genHuffmanCode $ createHuffmanTree $ genLeafs $ freqCnt f str []


genFromHuffman str tree = genFromCompress str tree where
	genFromCompress lst (Leaf x _) = x:genFromCompress lst tree
	genFromCompress [] _ = []
	genFromCompress (x:xs) (Node _ lt rt)
		| x == '0' = genFromCompress xs lt
		| otherwise = genFromCompress xs rt
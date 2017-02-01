import qualified Data.Map as Map 
import qualified Data.Maybe as Mb
import qualified Data.List as List

foo::Map.Map Char Int
foo = Map.fromList []

freqCnt [] cont = cont
freqCnt (x:xs) cont = freqCnt xs $ Map.insertWith (+) x 1 cont

data Tree = Leaf Char Int | Node Int Tree Tree deriving Show

t::Tree
t = (Node 12
	(Leaf 'a' 5)
	(Node 123
		(Leaf 'b' 1231)
		(Node 123
			(Leaf 'c' 12)
			(Leaf 'd' 123))))

instance Eq Tree where
	(Leaf _ x) == (Leaf _ y) = x == y
	(Node x _ _) == (Leaf _ y) = x == y
	(Leaf _ x) == (Node y _ _) = x == y
	(Node x _ _) == (Node y _ _) = x == y
	
instance Ord Tree where
	compare (Leaf _ x) (Leaf _ y) = compare x y
	compare (Node x _ _) (Leaf _ y) = compare x y
	compare (Leaf _ x) (Node y _ _) = compare x y
	compare (Node x _ _) (Node y _ _) = compare x y

genLeafs cont = map (\ (key, freq)-> (Leaf key freq)) $ Map.toList cont
	
createHuffmanTree lst = head $ helper $ List.sort lst where
	createNode l@(Leaf _ x)   r@(Leaf _ y) = (Node (x + y) r l)
	createNode l@(Node x _ _) r@(Leaf _ y) = (Node (x + y) r l)
	createNode l@(Leaf _ x)   r@(Node y _ _) = (Node (x + y) r l)
	createNode l@(Node x _ _) r@(Node y _ _) = (Node (x + y) r l)
	helper (x:xs)
		| null xs = [x]
		| otherwise = helper $ List.sort ((createNode x $ head xs) : tail xs)

genHuffmanCode:: Tree -> [(Char, String)]
genHuffmanCode (Node _ lt rt) = map (\ (x:xs)-> (x, xs))
	$ (genSingleCode lt "0") ++ (genSingleCode rt "1") where
	genSingleCode (Node _ lt rt) code = 
		(genSingleCode lt (code ++ "0") ) ++ (genSingleCode rt (code ++ "1"))
	genSingleCode (Leaf x _) code = [x:code]


genCompressed [] _ = []
genCompressed (x:xs) cont = (Mb.fromJust (Map.lookup x cont)) ++ genCompressed xs cont

compress str = genCompressed str $ Map.fromList
	$ genHuffmanCode $ createHuffmanTree $ genLeafs $ freqCnt str foo


genFromHuffman str tree = genFromCompress str tree where
	genFromCompress lst (Leaf x _) = x:genFromCompress lst tree
	genFromCompress [] _ = []
	genFromCompress (x:xs) (Node _ lt rt)
		| x == '0' = genFromCompress xs lt
		| otherwise = genFromCompress xs rt
module Main where
import Huffman
import System.Environment (getArgs)
import System.IO (writeFile)

-- програмата работи с конзолни параметри
-- първият параметър е операцията, която ще 
-- се изпълнява вариантите са: 
-- encode и decode
-- след което приема имена на входен и изходен файл,
-- ако операцията е encode, трябва да бъде даден 4-ти 
-- аргумент, който е типа на данните, които ще бъдат кодирани 
-- като в момента работи само за Int, а с всичко останало
-- като Char 
-- примери: 
-- main encode <source file> <destination file> <dataType>
-- main decode <source file> <destination file>
main = do 
	args <- getArgs
	if length args < 3 then
		putStrLn "Inappropriate number of arguments!"
	else do
		let (command:fileName:outFilename:xs) = args
		putStrLn $ "Operation: " ++ command
		putStrLn $ "Source: " ++ fileName
		putStrLn $ "Destination: " ++ outFilename
		
		if command == "encode" then do
			str <- readFile fileName
			let typeToCode = head xs
			if typeToCode == "Int" then do
				let a = (read str :: [Int])
				let res = compress (==) str
				writeFile outFilename $ typeToCode ++ "\n"
					++ (show (snd $ compress (==) a)) ++ "\n" ++ (fst $ compress (==) a)
			else do
				let res = compress (==) str
				writeFile outFilename $ typeToCode ++ "\n"
					++ (show (snd res)) ++ "\n" ++ (fst res)
		else do
			str <- readFile fileName
			let (marker:tree:cod:_) = lines str
			if marker == "Int" then do
				let t = (read tree :: Tree Int)
				writeFile outFilename $ show (genFromHuffman cod t)
			else do
				let t = (read tree :: Tree Char)
				writeFile outFilename $ (genFromHuffman cod t)
		putStrLn $ "done."
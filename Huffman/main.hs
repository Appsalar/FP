module Main where
import Huffman
import System.Environment (getArgs)
import System.IO (writeFile)

main = do 
	args <- getArgs
	if length args < 3 then
		putStrLn "Inappropriate number of arguments!"
	else do
		let (command:fileName:outFilename:_) = args
		putStrLn $ "Operation: " ++ command
		if command == "encode" then do
			putStrLn $ "Source: " ++ fileName
			str <- readFile fileName
			let a = (read str :: [Int])
			print a
			putStrLn $ "Destination: " ++ outFilename
			writeFile outFilename $ (show $ snd $ compress (==) a) 
				++ "\n" ++ (fst $ compress (==) a)
		else do
			putStrLn $ "Source: " ++ fileName
			str <- readFile fileName
			let a = lines str
			--print a
			putStrLn $ "Destination: " ++ outFilename
			let t = (read (head a) :: Tree Int)
			print $ show t
			let cod = a !! 1
			print cod
			writeFile outFilename $ show (genFromHuffman cod t)
		putStrLn $ "done."
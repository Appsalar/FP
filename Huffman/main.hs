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
		putStrLn $ "Source: " ++ fileName
		str <- readFile fileName
		putStrLn $ "Destination: " ++ outFilename
		writeFile outFilename $ (show $ snd $ compress (==) str) 
			++ "\n" ++ (fst $ compress (==) str)
		putStrLn $ "done."
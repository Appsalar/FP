module Main where
import Zoo
import System.IO
import System.Directory

animals:: Tree
animals = Question "Лети ли?"
            ((Question "Птица ли е?") (Animal "Гълъб") (Animal "Комар"))
            (Animal "Крава")

fileName:: String
fileName = "data.txt"


main = do 
		fileExists <- doesFileExist fileName
		if fileExists then do
			h <- openFile fileName ReadMode
			db <- hGetContents h
			let t = (read db :: Tree)
			newDb <- play t
			hClose h
			writeFile fileName $ show newDb
		else do
			newDb <- play animals
			writeFile fileName $ show newDb

module Zoo where
import System.IO

data Tree = Animal String | Question String Tree Tree deriving (Show, Read)
data Answer = Yes | No

ask :: String -> IO Answer
ask str = do 
		putStrLn $ str ++ " (да / не)"
		getAnswer

getAnswer :: IO Answer
getAnswer = do 
		ans <- getLine
		putStrLn ""
		case ans of
			"да" -> return Yes
			"не" -> return No
			_ -> do
					putStrLn "Ни стаа неверенико, пиши  \"да\" или \"не\" ..." 
					getAnswer

getNewAnimal :: Tree -> IO Tree
getNewAnimal animal@(Animal name) = do 
		newName <- getLine
		putStrLn name
		let newAnimal = Animal newName
		putStrLn $ "Как да различа " ++ 
			newName ++ " и " ++ name ++ " ?"
		question <- getLine
		putStrLn "Кое от двете отговаря с \"да\" на този въпрос?"
		order <- getLine
		if order == newName then
			return $ Question question newAnimal animal
		else 
			return $ Question question animal newAnimal
			
animalGuess :: Tree -> IO Tree
animalGuess question@(Question s y n) = do
		ans <- ask s
		case ans of
			Yes -> do 
					newYes <- animalGuess y
					return $ Question s newYes n
			No -> do
					newNo <- animalGuess n
					return $ Question s y newNo

animalGuess animal@(Animal name) = do 
		ans <- ask $ name ++ " ли е?"
		case ans of
			Yes -> do 
				putStrLn "Печеля. КЕК"  
				return animal
			No -> do 
				putStrLn "Предавам се какъв е отговорът?"
				getNewAnimal animal
					
play :: Tree -> IO Tree
play root = do 
		putStrLn "Робе, намисли си животно!"
		newRoot <- animalGuess root
		playAgain <- ask "Да те гърча отново??"
		case playAgain of
			Yes -> play newRoot
			No -> do 
						putStrLn "GG EZ"
						return newRoot

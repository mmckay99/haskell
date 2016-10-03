import System.Random
import Control.Monad

game :: IO()
game = do 
          gen <- newStdGen
          secret <- randomRIO(0, 100) :: IO Int
          putStrLn "How much wood would a woodchuck chuck if a woodchuck could chuck wood?"
          turn (secret+1) secret 0

-- The "turn" function...
turn :: Int -> Int -> Int -> IO()
turn guess n turnNumber =
  do if guess==n
     then putStrLn ("You got it in " ++ show turnNumber ++ " guesses!")
     else mkguess n (turnNumber + 1)

mkguess :: Int -> Int -> IO()
mkguess n turnNumber =
  do putStr "Enter your guess: "
     q <- getLine
     let guessNum = read q
     when (guessNum<n) $ putStrLn "higher..."
     when (guessNum>n) $ putStrLn "lower..."
     turn guessNum n turnNumber

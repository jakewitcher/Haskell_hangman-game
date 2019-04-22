module Main where

import Control.Monad  (forever)
import Data.Char      (toLower)
import Data.Maybe     (isJust)
import Data.List      (intersperse)
import System.Exit    (exitSuccess)
import System.Random  (randomRIO)
import System.IO


newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do 
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int 
minWordLength = 5 

maxWordLength :: Int 
maxWordLength = 9

gameWords :: IO WordList 
gameWords = do 
  (WordList aw) <- allWords 
  return $ WordList (filter gameLength aw)
  where gameLength w = 
          let l  = length (w :: String)
          in  l >= minWordLength
          &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do 
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String 
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
     ++ " Guessed so far: " ++ intersperse ',' guessed

freshPuzzle :: String -> Puzzle 
freshPuzzle word =
  Puzzle word (go word) []
  where go = map (\x -> const Nothing x)
          
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char =
  char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) char =
  char `elem` guesses

renderPuzzleChar :: Maybe Char -> Char 
renderPuzzleChar mc =
  case mc of
    Just a  -> a 
    Nothing -> '_'

fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle word filledInSoFar alreadyGuessed) char =
  Puzzle word newFilledInSoFar (char : alreadyGuessed)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar 
        newFilledInSoFar =
          zipWith (zipper char) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do 
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess _ guessed) =
  if (wrongGuesses p) > 7 
  then do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    playAgain
  else return ()

wrongGuesses :: Puzzle -> Int 
wrongGuesses (Puzzle _ filledIn guesses) =
  length [x |  x <- guesses, not $ x `elem` correctGuesses]
  where correctGuesses = foldr go [] filledIn 
        go c ls =
          case c of
            Just a -> a : ls
            Nothing -> ls

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar 
  then do 
    putStrLn "You win!"
    playAgain
  else return ()

playAgain :: IO ()
playAgain = do 
  putStrLn "Play again?"
  response <- getLine
  case response of
    "yes"   -> main
    _     -> exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> 
      putStrLn "Your guess must\
              \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
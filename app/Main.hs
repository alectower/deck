module Main where

import Data.List as L
import Data.Text
import Deck
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn
        "Usage: deck-exe [import file.csv deck-name] [deck-name [num-cards] [new-cards]]"
      putStrLn "Examples:"
      putStrLn
        "       deck-exe import file.csv mydeck # import csv to mydeck"
      putStrLn
        "       deck-exe mydeck                 # Review cards from mydeck; num-cards default is 20, new-cards default is 5"
      putStrLn
        "       deck-exe mydeck 10              # Review 10 cards from mydeck, with 15 new"
    args -> do
      let first = args !! 0
      case first of
        "import" -> processImport (L.drop 1 args)
        deck -> processDeck args

processImport :: [String] -> IO ()
processImport [file] = do
  putStrLn "Missing file or deck name"
  return ()
processImport [file, name] = importCsv file name
processImport _ = putStrLn "Wrong number of arguments"

processDeck :: [String] -> IO ()
processDeck [file, numCards, numNewCards] =
  cycleDeck
    (file L.++ ".json")
    (read numCards :: Int)
    (read numNewCards :: Int)
processDeck [file, numCards] =
  cycleDeck (file L.++ ".json") (read numCards :: Int) 5
processDeck [file] = cycleDeck (file L.++ ".json") 20 5

cycleDeck :: String -> Int -> Int -> IO ()
cycleDeck file numCards numNewCards
  | numCards < numNewCards = do
    putStrLn "num-cards must be greater than or equal to new-cards"
  | otherwise = do
    deck <- getDeck file
    let unReviewed = L.filter (\c -> numViews c == 0) deck
    let reviewed = deck L.\\ unReviewed
    let oldReview = L.take (numCards - numNewCards) $ sort reviewed
    let newReview = L.take numNewCards unReviewed
    let nonSessionDeck = deck L.\\ (oldReview L.++ newReview)
    updatedSessionDeck <- walkDeck [] $ newReview L.++ oldReview
    writeDeck file $ sort $ updatedSessionDeck L.++ nonSessionDeck

walkDeck :: Deck -> Deck -> IO Deck
walkDeck newDeck [] = return newDeck
walkDeck newDeck (x:xs) = do
  clearScreen
  ans <- frontInteraction x
  ord <- backInteraction x
  walkDeck (updateCard ans ord x : newDeck) xs

updateCard :: String -> String -> Card -> Card
updateCard ans ord c =
  saveAnswer ans . incNumViews . updateOrder ord $ c

frontInteraction :: Card -> IO String
frontInteraction c = do
  putStrLn $ dispFront c
  putStrLn ""
  putStr "answer: "
  hFlush stdout
  getLine

backInteraction :: Card -> IO String
backInteraction c = do
  clearScreen
  putStrLn $ dispBack c
  putStrLn ""
  putStrLn "hard: h; okay: o; easy: e"
  getLine

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"
  putStr "\ESC[10;H"

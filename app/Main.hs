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
        "Usage: deck-exe [import file.csv deck-name] [deck-name [num-cards]]"
      putStrLn "Examples:"
      putStrLn
        "       deck-exe import file.csv mydeck # import csv to mydeck"
      putStrLn
        "       deck-exe mydeck                 # Review cards from mydeck; default is 20"
      putStrLn
        "       deck-exe mydeck 10              # Review 10 cards from mydeck"
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
processDeck [file, numCards] =
  cycleDeck (file L.++ ".json") (read numCards :: Int)
processDeck [file] = cycleDeck (file L.++ ".json") 20

cycleDeck :: String -> Int -> IO ()
cycleDeck file numCards = do
  deck <- getDeck file
  newDeck <- sessionDeck numCards $ sort deck
  writeDeck file $ sort newDeck

sessionDeck :: Int -> Deck -> IO Deck
sessionDeck numCards origDeck = do
  newDeck <- walkDeck [] $ L.take numCards origDeck
  return $ newDeck L.++ (L.drop numCards origDeck)

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

module Main where

import Data.List
import Data.Text
import Deck
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show args
  let file = args !! 0
  deck <- getDeck file
  newDeck <- walkDeck [] $ sort deck
  writeDeck (sort newDeck) file
  --putStrLn $ show newDeck
  return ()

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

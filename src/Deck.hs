{-# LANGUAGE DeriveGeneric #-}

module Deck
  ( Deck
  , Card(..)
  , makeCard
  , dispFront
  , dispBack
  , saveAnswer
  , updateOrder
  , incNumViews
  , getDeck
  , writeDeck
  , importCsv
  ) where

import Data.Aeson as A
import Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 as U
import Data.Csv as Csv
import Data.List as L
import Data.Text as T
import Data.Vector as V
import GHC.Generics
import GHC.Word
import System.Directory
import Text.Regex.TDFA

data Card = Card
  { front :: Text
  , back :: Text
  , order :: Integer
  , numViews :: Integer
  , numCorrect :: Integer
  } deriving (Show, Generic)

instance FromJSON Card

instance ToJSON Card

instance Eq Card where
  Card {order = a, numViews = b, numCorrect = c} == Card { order = d
                                                         , numViews = e
                                                         , numCorrect = f
                                                         } =
    a == d && b == e && c == f

instance Ord Card where
  compare Card {order = a, numViews = b, numCorrect = c} Card { order = d
                                                              , numViews = e
                                                              , numCorrect = f
                                                              }
    | c /= f = compare c f
    | a /= d = compare a d
    | b /= e = compare b e
    | otherwise = EQ

type Deck = [Card]

makeCard :: String -> String -> Card
makeCard f b =
  Card
    { front = T.pack f
    , back = T.pack b
    , order = 1
    , numViews = 0
    , numCorrect = 0
    }

dispFront :: Card -> String
dispFront c = T.unpack $ front c

dispBack :: Card -> String
dispBack c = T.unpack $ back c

incNumViews :: Card -> Card
incNumViews c = c {numViews = numViews c + 1}

incNumCorrect :: Card -> Card
incNumCorrect c = c {numCorrect = numCorrect c + 1}

saveAnswer :: String -> Card -> Card
saveAnswer "" c = c
saveAnswer ans c
  | (lowerT $ back c) =~ (lower ans) = incNumCorrect c
  | otherwise = c

lower :: String -> String
lower = T.unpack . toLower . T.pack

lowerT :: Text -> String
lowerT = T.unpack . toLower

updateOrder :: String -> Card -> Card
updateOrder "e" c = c {order = 3}
updateOrder "o" c = c {order = 2}
updateOrder "h" c = c {order = 1}
updateOrder _ c = c

getDeck :: String -> IO Deck
getDeck file = do
  deckPath <- deckLocation
  s <- A.decode <$> BL.readFile (deckPath L.++ "/" L.++ file)
  case s of
    Just a -> return a
    Nothing -> return []

deckLocation :: IO String
deckLocation = do
  dir <- getHomeDirectory
  return $ dir L.++ "/.deck"

writeDeck :: String -> Deck -> IO ()
writeDeck file deck = do
  deckPath <- deckLocation
  let filePath = deckPath L.++ "/" L.++ file
  dirExists <- doesDirectoryExist deckPath
  case dirExists of
    False -> createDirectory deckPath
    True -> return ()
  BL.writeFile filePath $ A.encode deck

readCsv :: String -> IO (Vector (Vector BL.ByteString))
readCsv file = do
  f <- BL.readFile file
  let s =
        Csv.decode NoHeader f :: Either String (Vector (Vector BL.ByteString))
  case s of
    Right rows -> return rows
    Left _ -> return $ fromList []

importCsv :: String -> String -> IO ()
importCsv file name = do
  rows <- readCsv file
  let deckFile = name Prelude.++ ".json"
  writeDeck deckFile . rowsToDeck [] $ rows

rowsToDeck :: Deck -> Vector (Vector BL.ByteString) -> Deck
rowsToDeck newDeck rows
  | V.length rows == 0 = newDeck
  | otherwise =
    let front = U.toString $ toStrict $ rows ! 0 ! 0
        back =
          (lineBreak . lineBreak $ front) L.++
          joinCols (V.drop 1 $ rows ! 0)
     in rowsToDeck (makeCard front back : newDeck) $ V.drop 1 rows

joinCols :: Vector BL.ByteString -> String
joinCols cols = V.foldr concatCols "" cols

concatCols :: BL.ByteString -> String -> String
concatCols x y =
  (lineBreak . lineBreak $ (U.toString $ toStrict x)) L.++ y

lineBreak :: String -> String
lineBreak x = x L.++ "\n"

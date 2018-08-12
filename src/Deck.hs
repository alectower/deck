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
  ) where

import Data.Aeson
import Data.ByteString.Lazy as L
import Data.Text as T
import GHC.Generics
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
    | a /= d = compare a d
    | b /= e = compare b e
    | c /= f = compare c f
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
  s <- decode <$> L.readFile file
  case s of
    Just a -> return a
    Nothing -> return []

writeDeck :: Deck -> String -> IO ()
writeDeck deck file = L.writeFile file $ encode deck

module Card where

import Data.List
import Data.Maybe
import Text.Read (readEither, readMaybe)

------------------
--  Card model  --
------------------

type Card = (Rank, Suit)

data Face = Jack | Queen | King
    deriving (Eq, Show, Enum)

instance Ord Face where
    compare x y = compare (fromEnum x) (fromEnum y)

data Rank = Number { n :: Int }
          | Face   { f :: Face }
          | Ace
        deriving (Eq)

rankValue :: Rank -> Int
rankValue (Number x) = x
rankValue (Face x)   = 11 + fromEnum x
rankValue Ace        = 14

instance Ord Rank where
    compare x y = compare (rankValue x) (rankValue y)

instance Show Rank where
    show (Number x) = show x
    show (Face   x) = show x
    show Ace        = id "Ace"

data Suit = Hearts | Spades | Clubs | Diamonds
    deriving (Eq)

instance Show Suit where
    show Hearts   = "♥"
    show Spades   = "♠"
    show Clubs    = "♣"
    show Diamonds = "♦"

instance Ord Suit where
    compare _ _ = EQ

-------------------------
--  Card construction  --
-------------------------

card :: Rank -> Suit -> Card
card r s = (r, s)

readRank :: Char -> Maybe Rank
readRank r | r == 'A'        = Just Ace
           | r == 'J'        = Just (Face Jack)
           | r == 'Q'        = Just (Face Queen)
           | r == 'K'        = Just (Face King)
           | r == 'T'        = Just (Number 10)
           | n < 10 && n > 1 = Just (Number n)
           | otherwise       = Nothing
           where
               n = fromMaybe 0 (readMaybe [r] :: Maybe Int)

readSuit :: Char -> Maybe Suit
readSuit s | s == 'H'  = Just Hearts
           | s == 'S'  = Just Spades
           | s == 'C'  = Just Clubs
           | s == 'D'  = Just Diamonds
           | otherwise = Nothing

invalidCard = error "Invalid card"

readCard :: String -> Card
readCard s | length s /= 2 = invalidCard
           | otherwise     = do
               let rank = readRank (head s)
               let suit = readSuit (head $ tail s)
               if (rank /= Nothing && suit /= Nothing)
                  then card (case rank of Just x -> x)
                            (case suit of Just x -> x)
                  else invalidCard

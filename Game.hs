module Game where

import System.IO.Unsafe

import Card
import Hand

games = do
    let file = readFile "./poker.txt"
    let text = unsafePerformIO file
    lines text

extractHands :: String -> (Hand, Hand)
extractHands xs = (readHand (take 5 cs), readHand (drop 5 cs))
    where cs = words xs

outcomes = map ((uncurry compareHands) . extractHands) games

game = length $ filter (== True) outcomes

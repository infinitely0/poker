module Game where

import Card
import Hand

extractHands :: String -> (Hand, Hand)
extractHands xs = (readHand (take 5 cs), readHand (drop 5 cs))
    where cs = words xs

game :: IO ()
game = do
    file <- readFile "poker.txt"
    let games    = map extractHands $ lines file
        outcomes = map (uncurry compareHands) games
        won      = filter (== True) outcomes
    print $ length won

module Game where

import Card
import Hand

p1Hand = readHand ["8C", "8S", "8C", "8H", "2S"]
p2Hand = readHand ["2D", "2S", "2C", "2H", "AC"]

game = print $ compareHands p1Hand p2Hand

-- TODO
-- use assoc to `find` hands

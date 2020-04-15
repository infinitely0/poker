module Hand where

import Card
import Data.List
import qualified Data.Map.Strict as Map

------------------------
--  Hand manipulation --
------------------------

type Hand = [Card]

validHand :: [Card] -> Maybe Hand -- TODO check for duplicates etc.
validHand cs = if length cs == 5 && length (nub cs) == 5
                  then Just cs
                  else Nothing

readHand :: [String] -> Hand
readHand cs = if length cs == 5
                 then map readCard cs
                 else error "Invalid hand"

groupByRank :: [Card] -> [[Card]]
groupByRank []     = []
groupByRank (c:cs) = [c : matches] ++ groupByRank rest
    where
        matches = filter ((== fst c) . fst) cs
        rest    = filter (not . (`elem` matches)) cs

groupBySuit :: [Card] -> [[Card]]
groupBySuit [] = []
groupBySuit cs = do
    let (oc:ocs) = sortOn snd cs
        matches  = filter ((== snd oc) . snd) ocs
        rest     = filter (not . (`elem` matches)) ocs
    [oc : matches] ++ groupBySuit rest

isConsecutive :: [Int] -> Bool
isConsecutive xs =
    (length (nub xs) == length xs) && (head xs - last xs) == (length xs - 1)

-- Finds the highest card in a presorted straight, special ace rules apply
straightHigh :: Hand -> Maybe Card
straightHigh cs | values == [14, 5, 4, 3, 2]     = Just (find 5)
                | isConsecutive values           = Just (find (head values))
                | otherwise                      = Nothing
                where
                    values = map (rankValue . fst) cs
                    find x = head $ filter ((== x) . rankValue . fst) cs

-----------------------
--  Hand evaluation  --
-----------------------

data HandRank = HighCard | OnePair   | TwoPair     | ThreeOfAKind  | Straight
              | Flush    | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
    deriving (Eq, Ord, Show)

-- High Card: Highest value card.
-- One Pair: Two cards of the same value.
-- Two Pairs: Two different pairs.
-- Three of a Kind: Three cards of the same value.
-- Straight: All cards are consecutive values.
-- Flush: All cards of the same suit.
-- Full House: Three of a kind and a pair.
-- Four of a Kind: Four cards of the same value.
-- Straight Flush: All cards are consecutive values of same suit.
-- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

handScores :: Map.Map HandRank Integer
handScores = Map.fromList [(HighCard,      0),
                           (OnePair,       1),
                           (TwoPair,       2),
                           (ThreeOfAKind,  3),
                           (Straight,      4),
                           (Flush,         5),
                           (FullHouse,     6),
                           (FourOfAKind,   7),
                           (StraightFlush, 8),
                           (RoyalFlush,    9)]

highCard :: Hand -> Card
highCard = maximum

-- Returns only the relevant cards to break ties
evalHand :: Hand -> (HandRank, [Card])
evalHand cs | has flush && isRoyal      = (RoyalFlush,    [])
            | has straight && has flush = (StraightFlush, straight)
            | has fours                 = (FourOfAKind,   take 1 $ head fours)
            | has threes && has pairs   = (FullHouse,     take 1 $ head threes)
            | has flush                 = (Flush,         head flush)
            | has straight              = (Straight,      straight)
            | has threes                = (ThreeOfAKind,  head threes)
            | length pairs > 1          = (TwoPair,       head pairs)
            | has pairs                 = (OnePair,       head pairs)
            | otherwise                 = (HighCard,      [highCard cs])
            where
                ocs      = reverse (sort cs)
                ranks    = groupByRank ocs
                suits    = groupBySuit ocs

                flush    = filter ((== 5) . length) suits
                straight = case straightHigh ocs of
                             Just x  -> [x]
                             Nothing -> []

                isRoyal  = case straight of
                             []    -> False
                             [x]   -> fst x == Ace

                fours    = filter ((== 4) . length) ranks
                threes   = filter ((== 3) . length) ranks
                pairs    = filter ((== 2) . length) ranks

                has      = not . null

-- TODO allow draws (no draws in question)

-- True  = Player 1 wins
-- False = Player 1 loses
compareHands :: Hand -> Hand -> Bool
compareHands p1 p2 | p1Score == p2Score = resolve p1TieCs p2TieCs
                   | otherwise          = p1Score > p2Score
    where
        p1Eval = evalHand p1
        p2Eval = evalHand p2

        p1TieCs = snd p1Eval
        p2TieCs = snd p2Eval

        p1Score = Map.lookup (fst $ p1Eval) handScores
        p2Score = Map.lookup (fst $ p2Eval) handScores

-- Breaks ties if hand rank is equal
resolve :: [Card] -> [Card] -> Bool
resolve [] [] = False
resolve p1 p2 = do
    let p1Best = fst $ head p1
        p2Best = fst $ head p2
    if p1Best == p2Best
       then resolve (tail p1) (tail p2)
       else p1Best > p2Best

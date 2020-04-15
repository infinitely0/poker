import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Card
import Hand

main :: IO ()
main = hspec $ do
  describe "Card" $ do
    it "converts string to card" $ do
      readCard "2C" `shouldBe` ((Number 2), Clubs)
      readCard "KD" `shouldBe` ((Face King), Diamonds)
      readCard "AS" `shouldBe` (Ace, Spades)

    it "throws an exception for invalid cards" $ do
      evaluate (readCard "---") `shouldThrow` anyErrorCall
      evaluate (readCard "8F") `shouldThrow` anyErrorCall
      evaluate (readCard "1D") `shouldThrow` anyErrorCall

    it "converts rank to value" $ do
      let (r, _) = (readCard "AS")
      rankValue r `shouldBe` 14
      let (r, _) = (readCard "2S")
      rankValue r `shouldBe` 2
      let (r, _) = (readCard "KS")
      rankValue r `shouldBe` 13

    it "compares ranks" $ do
      compare (Face King) Ace `shouldBe` LT
      compare (Face King) (Number 10) `shouldBe` GT
      compare (Number 10) (Number 2) `shouldBe` GT
      compare Ace Ace `shouldBe` EQ
      compare (Number 10) (Number 10) `shouldBe` EQ

  describe "Hand" $ do
    it "reads hand and converts to cards" $ do
      let hand = [(Ace, Clubs), ((Number 2), Hearts), ((Number 3), Diamonds),
                  ((Number 10), Clubs), ((Face King), Spades)]
      readHand ["AC","2H","3D","TC","KS"] `shouldBe` hand

    it "finds high card" $ do
      let hand = readHand ["AC","2H","3D","TC","KS"]
      highCard hand `shouldBe` (Ace, Clubs)

    it "evalutes hand with high card" $ do
      let hand = readHand ["AC","2H","3D","TC","KS"]
      evalHand hand `shouldBe` (HighCard, [(Ace, Clubs)])

    it "evaluates hand with one pair" $ do
      let hand = readHand ["AC","AH","3D","TC","KS"]
      evalHand hand `shouldBe` (OnePair, [(Ace, Hearts), (Ace, Clubs)])

    it "evaluates hand with two pairs" $ do
      let hand = readHand ["2C","3H","2D","3C","KS"]
      evalHand hand `shouldBe` (TwoPair, [((Number 3), Clubs),
                                          ((Number 3), Hearts)])

    it "evaluates hand with three of a kind" $ do
      let hand = readHand ["2C","3H","2D","8C","2S"]
      evalHand hand `shouldBe` (ThreeOfAKind, [((Number 2), Spades),
                                               ((Number 2), Diamonds),
                                               ((Number 2), Clubs)])

    it "evaluates hand with three of a kind" $ do
      let hand = readHand ["2C","3H","2D","8C","2S"]
      evalHand hand `shouldBe` (ThreeOfAKind, [((Number 2), Spades),
                                               ((Number 2), Diamonds),
                                               ((Number 2), Clubs)])

    it "evaluates hand with straight" $ do
      let hand = readHand ["2C","3H","4D","5C","6S"]
      evalHand hand `shouldBe` (Straight, [((Number 6), Spades)])

    it "evaluates hand with baby straight" $ do
      let hand = readHand ["2C","3H","4D","5C","AS"]
      evalHand hand `shouldBe` (Straight, [((Number 5), Clubs)])

    it "evaluates hand with flush" $ do
      let hand = readHand ["2S","3S","4S","KS","JS"]
      evalHand hand `shouldBe` (Flush, [((Face King), Spades),
                                        ((Face Jack), Spades),
                                        ((Number 4), Spades),
                                        ((Number 3), Spades),
                                        ((Number 2), Spades)])

      let hand = readHand ["2S","3S","4S","8S","9S"]
      evalHand hand `shouldBe` (Flush, [((Number 9), Spades),
                                        ((Number 8), Spades),
                                        ((Number 4), Spades),
                                        ((Number 3), Spades),
                                        ((Number 2), Spades)])

    it "evaluates hand with full house" $ do
      let hand = readHand ["2S","2C","4S","4C","4H"]
      evalHand hand `shouldBe` (FullHouse, [((Number 4), Hearts)])

    it "evaluates hand with four of a kind" $ do
      let hand = readHand ["2S","2C","2H","2D","4H"]
      evalHand hand `shouldBe` (FourOfAKind, [((Number 2), Diamonds)])

    it "evaluates hand with straight flush" $ do
      let hand = readHand ["2C","3C","4C","5C","6C"]
      evalHand hand `shouldBe` (StraightFlush, [((Number 6), Clubs)])

    it "evaluates hand with royal flush" $ do
      let hand = readHand ["AC","KC","QC","JC","TC"]
      evalHand hand `shouldBe` (RoyalFlush, [])

  describe "Game" $ do
    it "identifies winner of game" $ do
      let p1Hand = readHand ["KC", "KS", "KC", "KH", "9S"]
      let p2Hand = readHand ["7D", "2S", "5D", "3S", "AC"]
      compareHands p1Hand p2Hand `shouldBe` True

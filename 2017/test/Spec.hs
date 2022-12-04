import Test.Hspec

import AdventOfCode

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    describe "captcha1" $ do
      it "works for the first example" $ do
        captcha1 "1122" `shouldBe` "3"

      it "works for the second example" $ do
        captcha1 "1111" `shouldBe` "4"

      it "works for the third example" $ do
        captcha1 "1234" `shouldBe` "0"

      it "works for the fourth example" $ do
        captcha1 "91212129" `shouldBe` "9"

    describe "captcha2" $ do
      it "works for the first example" $ do
        captcha2 "1212" `shouldBe` "6"

      it "works for the second example" $ do
        captcha2 "1221" `shouldBe` "0"

      it "works for the third example" $ do
        captcha2 "123425" `shouldBe` "4"

      it "works for the fourth example" $ do
        captcha2 "123123" `shouldBe` "12"

      it "works for the fifth example" $ do
        captcha2 "12131415" `shouldBe` "4"
  describe "Day 2" $ do
    describe "checksum" $ do
      it "works for the given example" $ do
        checksum "5 1 9 5\n7 5 3\n2 4 6 8" `shouldBe` "18"
    describe "calculateRow" $ do
      it "works for the first row" $ do
        calculateRow [5, 9, 2, 8] `shouldBe` 4
      it "works for the second row" $ do
        calculateRow [9, 4, 7, 3] `shouldBe` 3
      it "works for the third row" $ do
        calculateRow [3, 8, 6, 5] `shouldBe` 2
    describe "sumEvenlyDivisibleValues" $ do
      it "works for the example input" $ do
        sumEvenlyDivisibleValues "5 9 2 8\n9 4 7 3\n3 8 6 5" `shouldBe` "9"
  describe "Day 3" $ do
    describe "memory" $ do
      it "contains the right values of distance" $ do
        memory !! 2 `shouldBe` 1
        memory !! 11 `shouldBe` 2
        memory !! 28 `shouldBe` 3
        memory !! 53 `shouldBe` 4
        memory !! 86 `shouldBe` 5
        memory !! 77 `shouldBe` 4
        memory !! 75 `shouldBe` 6


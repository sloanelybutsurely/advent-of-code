module Day5 (BoardingPass(BoardingPass), seatId) where

data BoardingPass = BoardingPass { row :: Int, column :: Int }
  deriving (Show)

binaryStringToInt b = binaryStringToInt' (reverse b) 0 0
  where
    binaryStringToInt' [] _ acc = acc
    binaryStringToInt' ('1':bs) m acc = binaryStringToInt' bs (m+1) (acc + 2^m)
    binaryStringToInt' ('0':bs) m acc = binaryStringToInt' bs (m+1) acc

charToBinaryDigit 'F' = '0'
charToBinaryDigit 'L' = '0'
charToBinaryDigit 'B' = '1'
charToBinaryDigit 'R' = '1'

stringToInt = binaryStringToInt . map charToBinaryDigit

readBoardingPass str =
    let (str', rest) = splitAt 10 str
        (rowS, colS) = splitAt 7 str'
    in
        (BoardingPass (stringToInt rowS) (stringToInt colS), rest)

instance Read BoardingPass where
  readsPrec _ str = [readBoardingPass str]

seatId (BoardingPass row column) = row * 8 + column

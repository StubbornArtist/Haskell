module AshleyCurrie2 where

data Piece = Yellow | Red | Blank
 deriving(Eq)
 
instance Show Piece where
 show Red = "Red"
 show Yellow = "Yellow"
 show Blank = "Blank"
  
type Column = [Piece]
type Board = [Column]

data BoardState = BS {
      theBoard :: Board,
      lastMove :: Piece,
      numColumns :: Int,
      numRows :: Int,
      numToConnect :: Int
} 

--general list operations 
len :: [a] -> Int 
len [] = 0
len (x : xs) = 1 + len xs

insertAt :: [a] -> Int -> a -> [a]
insertAt xs 0 x = (x : xs)
insertAt (x0 : xs) n x = x0 : insertAt xs (n - 1) x

replaceAt :: [a] -> Int -> a -> [a]
replaceAt (x0 : xs) 0 x = (x : xs)
replaceAt (x0 : xs) n x = x0 : replaceAt xs (n - 1) x 

elementAt :: [a] -> Int -> a
elementAt (x : xs) 0 = x
elementAt (x : xs) n = elementAt xs (n - 1)

appendTo :: [a] -> a -> [a]
appendTo xs x = insertAt xs (len xs) x

takeFront :: [[a]] -> [a]
takeFront [(x : xs)] = [x]
takeFront ((x : xs) : xss) = x : takeFront xss 

removeFront :: [[a]] -> [[a]]
removeFront [(x : xs)] = [xs]
removeFront ((x : xs) : xss) = xs : removeFront xss 

transposeRev :: [[a]] -> [[a]]
transposeRev ([] : xs) = []
transposeRev xs = appendTo (transposeRev (removeFront xs)) (takeFront xs)

transposeNorm :: [[a]] -> [[a]]
transposeNorm ([] : xs) = []
transposeNorm xs = (takeFront xs) : transposeNorm (removeFront xs)

removePiece :: [Maybe Piece] -> Maybe Piece -> [Maybe Piece]
removePiece [] p = []
removePiece (x : xs) p = if x == p then removePiece xs p
                        else x : removePiece xs p
                        
removePiece2D :: [[Maybe Piece]] -> Maybe Piece -> [[Maybe Piece]]
removePiece2D [] p = []
removePiece2D (x : xs) p = removePiece x p : removePiece2D xs p

appendNum :: [a] -> Int -> a -> [a]
appendNum xs 0 x = xs
appendNum xs n x = appendTo (appendNum xs (n - 1) x) x 

pushNum :: [a] -> Int -> a -> [a]
pushNum xs 0 x = xs
pushNum xs n x = x : pushNum xs (n - 1) x

padSeq :: [[Maybe Piece]] -> Int -> Int -> Bool -> [[Maybe Piece]]
padSeq [] n m b = []
padSeq (x : xs) n m b = if b then (pushNum (appendNum x n (Just Blank)) m (Just Blank)): padSeq xs (n - 1)(m + 1) b
                        else (pushNum (appendNum x n (Just Blank)) m (Just Blank)): padSeq xs (n + 1)(m - 1) b
  
--tells you if a specified column in the board is full
columnFull :: BoardState -> Int -> Bool
columnFull b cNum = len(elementAt (theBoard b) cNum) == (numRows b)
        
--tells you the current piece to be played               
currentMove :: BoardState -> Piece
currentMove b = if (lastMove b) == Yellow then Red
                else Yellow 
                
--add the current piece to the board at a given column             
addPiece :: BoardState -> Int -> BoardState
addPiece b n = b {theBoard = replaceAt (theBoard b) n (appendTo(elementAt(theBoard b) n) (currentMove b)),
                  lastMove = currentMove b}

makeMove :: BoardState -> Int -> Maybe BoardState
makeMove b cNum  = if cNum > (numColumns b)
                   then Nothing
                   else if (columnFull b cNum)
                   then Nothing
                   else if cNum < 0
                   then Nothing
                   else Just (addPiece b cNum)

--creates a list of length n from a pre-existing list of length <= n
--where values do no exist in the input list the value Nothing will be placed in the new list
fullColumn :: Column -> Int -> [Maybe Piece]
fullColumn [] 0 = [Nothing]
fullColumn c 0 = [Just (elementAt c 0)]
fullColumn c n = if (n + 1) > len c then appendTo(fullColumn c (n - 1)) Nothing
                        else appendTo(fullColumn c (n - 1)) (Just (elementAt c n))

--creates a square 2d list from a 
fullBoard :: Board -> Int -> Int -> [[Maybe Piece]]
fullBoard b 0 h = []
fullBoard b w h = if w > len b then appendTo (fullBoard b (w - 1) h) (fullColumn [] h)
                  else appendTo (fullBoard b (w - 1) h) (fullColumn(elementAt b (w - 1)) h)
                  
columns :: BoardState -> [[Maybe Piece]]
columns b = fullBoard (theBoard b) (numColumns b) ((numRows b) - 1)

rows :: BoardState -> [[Maybe Piece]]
rows b = transposeRev (columns b)  

diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward b = removePiece2D(transposeNorm (padSeq (rows b) ((numColumns b) - 1) 0 True)) (Just Blank)
 
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward b = removePiece2D(transposeNorm (padSeq (rows b) 0 ((numColumns b) - 1) False)) (Just Blank)

consec :: [Maybe Piece] -> Maybe Piece -> Int -> Int -> Bool
consec [] prev num count = if count >= num then True else False
consec (x : xs) prev num count = if count >= num then True 
                                 else if x == prev then consec xs prev num (count + 1)
                                 else consec xs prev num 0
                                 
containsStringOf :: [[Maybe Piece]] -> Maybe Piece -> Int -> Bool
containsStringOf [] p n = False
containsStringOf (x : xs) p n = (consec x p n 0) || containsStringOf xs p n

win :: BoardState -> Bool
win b = containsStringOf (rows b) (Just(lastMove b)) (numToConnect b) ||
        containsStringOf (columns b) (Just(lastMove b)) (numToConnect b) ||
        containsStringOf (diagonalsForward b) (Just(lastMove b)) (numToConnect b)||
        containsStringOf (diagonalsBackward b) (Just(lastMove b)) (numToConnect b)

checkWin :: BoardState -> Maybe Piece
checkWin b = if win b then Just(lastMove b)
                else Nothing
  
bs :: BoardState
bs = BS {
 theBoard = [[Yellow],[Red],[Yellow, Yellow, Red],[Yellow, Red],[Red]],
 lastMove = Red,
 numColumns = 5 ,
 numRows = 4,
 numToConnect = 3
}


                           




                      


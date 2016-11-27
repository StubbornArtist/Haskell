module AshleyCurrie where

data Piece = Yellow|Red
 deriving(Eq)

type Column = [Piece]

type Board = [Column]

data BoardState = BS {
      theBoard :: Board,
      lastMove :: Piece,
      numColumns :: Int,
      numRows :: Int,
      numToConnect :: Int
}
len :: [a] -> Int
len xs = undefined

insertAt :: [a] -> Int -> a -> [a]
insertAt xs n x = undefined

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs n x = undefined

elementAt :: [a] -> Int -> a
elementAt xs n = undefined

appendTo :: [a] -> a -> [a]
appendTo xs x = insertAt xs (len xs) x
  

columnFull :: BoardState -> Int -> Bool
columnFull b cNum = len(elementAt(theBoard b) cNum) == numRows b

currentMove :: BoardState -> Piece
currentMove b = if (lastMove b) == Yellow then Red
                else Yellow 
                        
addPiece :: BoardState -> Int -> BoardState
addPiece b n = b {theBoard = replaceAt (theBoard b) n (appendTo(elementAt(theBoard b) n) (currentMove b)),
                  lastMove = currentMove b}

makeMove :: BoardState -> Int -> Maybe BoardState

makeMove b cNum  = if cNum > (numColumns b)
                    then Nothing
                   else if (columnFull b cNum)
                    then Nothing
                   else Just (addPiece b cNum)
                   
                   
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward b = undefined

diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward b = undefined

column :: BoardState -> [[Maybe Piece]]
column =  undefined

row :: BoardState -> [[Maybe Piece]]
row = undefined 

containsStringOf ::  [[Maybe Piece]] -> Maybe Piece -> Int -> Bool
containsStringOf = undefined
             
win :: BoardState -> Bool                
win b = containsStringOf (row b) (Just(lastMove b)) (numToConnect b) ||
        containsStringOf (column b) (Just(lastMove b)) (numToConnect b) ||
        containsStringOf (diagonalsForward b) (Just(lastMove b)) (numToConnect b)||
        containsStringOf (diagonalsBackward b) (Just(lastMove b)) (numToConnect b)       

checkWin :: BoardState -> Maybe Piece
checkWin b = if win b then Just(lastMove b)
                else Nothing




                        
                        
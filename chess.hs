import Data.Char

--------------------------------------------------------
-------- Useful array operations -----------------------
--------------------------------------------------------

-- 2d Array element copy
move :: (Int, Int) -> (Int, Int) -> [[Maybe a]] -> [[Maybe a]]
move (x, y) (x2, y2) board = fullMat
  where
    elem = (board !! y) !! x
    fullMat = replace y2 fullRow emptyMat
    fullRow = replace x2 elem (emptyMat !! y2) 
    emptyMat = replace y emptyRow board
    emptyRow = replace x Nothing (board !! y)

-- Replace an element in an 1d array
replace :: Int -> a -> [a] -> [a]
replace i x xs = left ++ (x : (tail right))
  where
    (left, right) = splitAt i xs 

get :: (Int, Int) -> [[a]] -> a
get (c, r) xss = (xss !! r) !! c

--------------------------------------------------------
--------------------------------------------------------

data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King
           deriving (Show, Eq)


data Color = White | Black
           deriving (Show, Eq)


data ColoredPiece = Create Color Piece
                  deriving (Show, Eq)


type MColoredPiece = Maybe ColoredPiece


type Board = [[MColoredPiece]]


showPiece :: Piece -> Char
showPiece Pawn = 'P'
showPiece Rook = 'R'
showPiece Knight = 'N'
showPiece Bishop = 'B'
showPiece Queen = 'Q'
showPiece King = 'K'


setColor :: Color -> Char -> Char
setColor White = toUpper
setColor Black = toLower


showColoredPiece :: ColoredPiece -> Char
showColoredPiece (Create color piece) =
  setColor color (showPiece piece) 


showMColoredPiece :: MColoredPiece -> Char
showMColoredPiece Nothing = ' '
showMColoredPiece (Just (Create color piece)) =
  showColoredPiece (Create color piece)


createMColoredPieces :: Color -> [Piece] -> [MColoredPiece]
createMColoredPieces color = map (\p -> Just (Create color p)) 


pawnMoves :: (Int, Int) -> Board -> [(Int, Int, MColoredPiece)]
pawnMoves (c, r) board = [
  (c + 1, r, Nothing),
  (c + 1, r + 1, get (c + 1, r + 1) board),
  (c + 1, r - 1, get (c + 1, r - 1) board)
  ] ++ (if (r == 1 || r == 6) then [(c + 2, r, Nothing)] else [])
  

initBoard :: Board
initBoard = [
  createMColoredPieces Black [Rook,
                             Knight,
                             Bishop,
                             Queen,
                             King,
                             Bishop,
                             Knight,
                             Rook],
  replicate 8 (Just (Create Black Pawn)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Create White Pawn)),
  
  createMColoredPieces White [Rook,
                             Knight,
                             Bishop,
                             Queen,
                             King,
                             Bishop,
                             Knight,
                             Rook]
  ]


showBoard :: Board -> String
showBoard = unlines . (map . map) showMColoredPiece 


printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


main = do
  print "Hello"

import Data.Char
import Data.Maybe
import Control.Monad

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


checkCoords :: (Int, Int) -> Maybe (Int, Int)
checkCoords (c, r)
  | c >= 0 && c < 8 && r >= 0 && r < 8 = Just (c, r)
  | otherwise = Nothing


getPiece :: (Int, Int) -> Board -> MColoredPiece
getPiece coords board = checkCoords coords >>= (flip get) board


removeNothing :: [Maybe a] -> [a]
removeNothing = map fromJust . filter isJust


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
showMColoredPiece (Just x) = showColoredPiece x


createMColoredPieces :: Color -> [Piece] -> [MColoredPiece]
createMColoredPieces color = map (Just . Create color)


getColor :: (Int, Int) -> Board -> Maybe Color
getColor coord board = fmap (\(Create color _) -> color) (get coord board)


isWhite :: (Int, Int) -> Board -> Bool
isWhite coord board = case getColor coord board of
  Just White -> True
  _ -> False


isBlack :: (Int, Int) -> Board -> Bool
isBlack coord board = case getColor coord board of
  Just Black -> True
  _ -> False


moves :: (Int, Int) -> Board -> Maybe [((Int, Int), MColoredPiece)]
moves coord board = case getPiece coord board of
  Just (Create color piece) -> case piece of
    Pawn -> Just (pawnMoves coord color board)
    Rook -> Just (rookMoves coord color board)
    _ -> Nothing
  _ -> Nothing


checkMove :: ((Int, Int), MColoredPiece) -> Maybe ((Int, Int), MColoredPiece)
checkMove (coords, mp) = fmap (const (coords, mp)) $ checkCoords coords


validMoves :: (Int, Int) -> Board -> Maybe [((Int, Int), MColoredPiece)]
validMoves = fmap (removeNothing . map checkMove) .: moves

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-------------------- Piece moves -------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

(.:) = (.) . (.)


rookMoves :: (Int, Int) -> Color -> Board -> [((Int, Int), MColoredPiece)]
rookMoves (c, r) color board = up ++ down ++ left ++ right
  where
    up = map (\coords -> (coords, getPiece coords board)) [(c, r - i) | i <- [1..8], isJust (getPiece (c, r - (i-1)) board)]
    down = map (\coords -> (coords, getPiece coords board)) [(c, r + i) | i <- [1..8], isJust (getPiece (c, r + (i-1)) board)]
    left = map (\coords -> (coords, getPiece coords board)) [(c - i, r) | i <- [1..8], isJust (getPiece (c - (i-1), r) board)]
    right = map (\coords -> (coords, getPiece coords board)) [(c + i, r) | i <- [1..8], isJust (getPiece (c + (i-1), r) board)]



pawnMoves :: (Int, Int) -> Color -> Board -> [((Int, Int), MColoredPiece)]
pawnMoves (c, r) color board =
  forwardOnceMove ++
  forwardOnceLeft ++
  forwardOnceRight ++
  forwardTwiceMove
    where
      forward = (c, r + one)
      forwardLeft = (c + 1, r + one)
      forwardRight = (c - 1, r + one)
      forwardTwice = (c, r + one * 2)
      forwardPiece = getPiece forward board
      forwardTwicePiece = getPiece forwardTwice board

      -- Check if there is a piece to kill
      forwardOnceLeft =
        maybeToList $ fmap ((,) forwardLeft . Just) (getPiece forwardLeft board)

      -- Check if there is a piece to kill
      forwardOnceRight =
        maybeToList $ fmap ((,) forwardRight . Just) (getPiece forwardRight board)

      -- Check if there is enough space
      forwardOnceMove = if isNothing forwardPiece
                        then [(forward, Nothing)] else []

      -- Check if there is enough space
      forwardTwiceMove =
        if (r == 1 || r == 6) &&
           isNothing forwardTwicePiece &&
           isNothing forwardPiece
        then [(forwardTwice, Nothing)] else []

      -- Convert color to direction
      one = if color == White then -1 else 1

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

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
  let a = initBoard
  let b = move (3, 1) (3, 5) a
  printBoard b
  print $ validMoves (3, 5) b

import Data.Char
import Data.Maybe
import Control.Monad

--------------------------------------------------------
--------------------------------------------------------

-- Double composition
(.:) = (.) . (.)


move :: (Int, Int) -> (Int, Int) -> [[Maybe a]] -> [[Maybe a]]
move (x, y) (x2, y2) board = fullMat
  where
    elem = (board !! y) !! x
    fullMat = replace y2 fullRow emptyMat
    fullRow = replace x2 elem (emptyMat !! y2)
    emptyMat = replace y emptyRow board
    emptyRow = replace x Nothing (board !! y)


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

data Direction = N
               | S
               | E
               | W
               | NE
               | NW
               | SE
               | SW
               deriving (Show, Eq)


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
createMColoredPieces = map . (Just .: Create)


extractColor :: MColoredPiece -> Maybe Color
extractColor = fmap (\(Create color _) -> color)


validMoves :: (Int, Int) -> Board -> Maybe [((Int, Int), MColoredPiece)]
validMoves = fmap (removeNothing . map checkMove) .: moves
  where
    -- Check the bounds
    checkMove (coords, mp) = fmap (const (coords, mp)) $ checkCoords coords
    -- Map the piece type to the specific function
    moves coord board = case getPiece coord board of
      Just (Create color piece) -> case piece of
        Pawn -> Just (pawnMoves coord color board)
        Rook -> Just (rookMoves coord color board)
        Bishop -> Just (bishopMoves coord color board)
        _ -> Nothing
      _ -> Nothing

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-------------------- Piece moves -------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------


collectPieces :: Board -> Color -> Direction -> Int -> (Int, Int) -> [((Int, Int), MColoredPiece)]
collectPieces board ownColor dir n (c, r) = map (\p -> (p, getPiece p board)) . map halfTuple $
                                            takeWhile verifyPosition dir2HistoricCoords
  where
    halfTuple (_,_,x,y) = (x, y)

    -- Create a list of historic positions
    dir2HistoricCoords = map unpack $ zip stream (tail stream)
      where
        unpack ((a, b), (c, d)) = (a, b, c, d)
        stream = dir2Coords dir (c, r) n
        dir2Coords N (c, r) n = [(c, r - i) | i <- [0..n]]
        dir2Coords S (c, r) n = [(c, r + i) | i <- [0..n]]
        dir2Coords W (c, r) n = [(c - i, r) | i <- [0..n]]
        dir2Coords E (c, r) n = [(c + i, r) | i <- [0..n]]
        dir2Coords NE (c, r) n = [(c + i, r - i) | i <- [0..n]]
        dir2Coords NW (c, r) n = [(c - i, r - i) | i <- [0..n]]
        dir2Coords SE (c, r) n = [(c + i, r + i) | i <- [0..n]]
        dir2Coords SW (c, r) n = [(c - i, r + i) | i <- [0..n]]

    -- Verifies the position
    verifyPosition  (oc, or, c, r) =
      noPiece || (color /= ownColor && (noOldPiece || oldColor == ownColor))
      where
        coords = (c, r)
        oldCoords = (oc, or)

        piece = getPiece coords board
        noPiece = isNothing piece

        oldPiece = getPiece oldCoords board
        noOldPiece = isNothing oldPiece

        color = fromJust $ extractColor piece
        oldColor = fromJust $ extractColor oldPiece


rookMoves :: (Int, Int) -> Color -> Board -> [((Int, Int), MColoredPiece)]
rookMoves coords color board = up ++ down ++ left ++ right
  where
    positions dir = collectPieces board color dir 8 coords
    up = positions N
    down = positions S
    left = positions W
    right = positions E


bishopMoves :: (Int, Int) -> Color -> Board -> [((Int, Int), MColoredPiece)]
bishopMoves coords color board = ne ++ nw ++ se ++ sw
  where
    positions dir = collectPieces board color dir 8 coords
    ne = positions NE
    nw = positions NW
    se = positions SE
    sw = positions SW


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

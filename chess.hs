import Data.Char
import Data.Maybe
import Control.Monad

--------------------------------------------------------
--------------------------------------------------------

-- Double composition
(.:) = (.) . (.)


move :: [[Maybe a]] -> (Int, Int) -> (Int, Int) -> [[Maybe a]]
move board (x, y) (x2, y2) = fullMat
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


get :: [[a]] -> (Int, Int) -> a
get xss (c, r)= (xss !! r) !! c


checkCoords :: (Int, Int) -> Maybe (Int, Int)
checkCoords (c, r)
  | c >= 0 && c < 8 && r >= 0 && r < 8 = Just (c, r)
  | otherwise = Nothing


getPiece :: Board -> (Int, Int) -> MColoredPiece
getPiece board coords = checkCoords coords >>= get board


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


validMoves :: Board -> (Int, Int) -> Maybe [((Int, Int), MColoredPiece)]
validMoves board = (fmap (removeNothing . map checkMove)) . buildMoveFunc
  where
    -- Check the bounds
    checkMove (coords, mp) = fmap (const (coords, mp)) $ checkCoords coords

    -- Map the piece type to the specific function
    moveFunc Pawn = pawnMoves
    moveFunc Rook = rookMoves
    moveFunc Bishop = bishopMoves
    moveFunc Queen = queenMoves
    moveFunc King = kingMoves
    moveFunc Knight = knightMoves

    -- fmap the coords to all possible moves for the given piece
    buildMoveFunc coords = fmap (\ (Create color piece) -> moveFunc piece board color coords) $
                           getPiece board coords


----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-------------------- Piece moves -------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Universal movement for long running pieces
collectPieces :: Board -> Color -> Direction -> Int -> (Int, Int) -> [((Int, Int), MColoredPiece)]
collectPieces board ownColor dir n (c, r) =
  map (\p -> (p, getPiece board p)) . map halfTuple $
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

        piece = getPiece board coords
        noPiece = isNothing piece

        oldPiece = getPiece board oldCoords
        noOldPiece = isNothing oldPiece

        color = fromJust $ extractColor piece
        oldColor = fromJust $ extractColor oldPiece


rookMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
rookMoves board color coords = up ++ down ++ left ++ right
  where
    positions dir = collectPieces board color dir 8 coords
    up = positions N
    down = positions S
    left = positions W
    right = positions E


bishopMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
bishopMoves board color coords = ne ++ nw ++ se ++ sw
  where
    positions dir = collectPieces board color dir 8 coords
    ne = positions NE
    nw = positions NW
    se = positions SE
    sw = positions SW


queenMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
queenMoves board color coords = horiVerti ++ diagonal
  where
    horiVerti = rookMoves board color coords
    diagonal = bishopMoves board color coords


kingMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
kingMoves board color coords = n ++ s ++ e ++ w ++ ne ++ nw ++ se ++ sw
  where
    positions dir = collectPieces board color dir 1 coords
    n = positions N
    s = positions S
    e = positions E
    w = positions W
    ne = positions NE
    nw = positions NW
    se = positions SE
    sw = positions SW


knightMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
knightMoves board color coords = undefined


pawnMoves :: Board -> Color -> (Int, Int) -> [((Int, Int), MColoredPiece)]
pawnMoves  board color (c, r) =
  forwardOnceMove ++
  forwardOnceLeft ++
  forwardOnceRight ++
  forwardTwiceMove
    where
      forward = (c, r + one)
      forwardLeft = (c + 1, r + one)
      forwardRight = (c - 1, r + one)
      forwardTwice = (c, r + one * 2)
      forwardPiece = getPiece board forward
      forwardTwicePiece = getPiece board forwardTwice

      -- Check if there is a piece to kill
      forwardOnceLeft =
        maybeToList $ fmap ((,) forwardLeft . Just) (getPiece board forwardLeft)

      -- Check if there is a piece to kill
      forwardOnceRight =
        maybeToList $ fmap ((,) forwardRight . Just) (getPiece board forwardRight)

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
  let b = move a (3, 1) (3, 5)
  printBoard b
  print $ validMoves b (3, 5)

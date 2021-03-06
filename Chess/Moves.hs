module Chess.Moves where

import Utils
import Chess.Types

import Data.Maybe
import Control.Monad


validMoves :: Board -> (Int, Int) -> Maybe [((Int, Int), MColoredPiece)]
validMoves board = (fmap $ removeNothing . map checkMove) . buildMoveFunc
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
    buildMoveFunc coords =
      fmap (\ (Create color piece) -> moveFunc piece board color coords) $
      getPiece board coords


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
        extractColor = fmap pieceColor

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
knightMoves board color (c, r) =
  position upLeft ++
  position upRight ++
  position downLeft ++
  position downRight ++
  position leftUp ++
  position leftDown ++
  position rightUp ++
  position rightDown

  where
    up = -1
    up2 = 2 * up
    down = 1
    down2 = 2 * down
    left = -1
    left2 = 2 * left
    right = 1
    right2 = 2 * right

    upLeft = (c + left, r + up2)
    upRight = (c + right, r + up2)

    downLeft = (c + left, r + down2)
    downRight = (c + right, r + down2)

    rightUp = (c + right2, r + up)
    rightDown = (c + right2, r + down)

    leftUp = (c + left2, r + up)
    leftDown = (c + left2, r + down)

    position coords = if hasEnemy && color == enemyColor
                      then [] else [(coords, enemy)]
      where
        enemy = getPiece board coords
        enemyColor = fromJust $ fmap pieceColor enemy
        hasEnemy = isJust enemy


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

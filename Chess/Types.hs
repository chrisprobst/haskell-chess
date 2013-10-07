module Chess.Types where

import Utils

import Data.List
import Data.Maybe
import Data.Char


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


data ColoredPiece = Create {
  pieceColor :: Color,
  pieceType :: Piece
  } deriving (Show, Eq)


type MColoredPiece = Maybe ColoredPiece


type Board = [[MColoredPiece]]


getPiece :: Board -> (Int, Int) -> MColoredPiece
getPiece board coords = checkCoords coords >>= get board


movePiece :: (Board, [ColoredPiece]) -> (Int, Int) -> (Int, Int) -> (Board, [ColoredPiece])
movePiece (board, pieces) from to = (nextBoard, nextPieces)
  where
    (nextBoard, mPiece) = move board from to
    nextPieces = maybe pieces (flip delete $ pieces) mPiece


showMColoredPiece :: MColoredPiece -> Char
showMColoredPiece = maybe ' ' showColoredPiece
  where
    showColoredPiece (Create color piece) = color2Case color (showPiece piece)

    color2Case White = toUpper
    color2Case Black = toLower

    showPiece Pawn = 'P'
    showPiece Rook = 'R'
    showPiece Knight = 'N'
    showPiece Bishop = 'B'
    showPiece Queen = 'Q'
    showPiece King = 'K'


initBoard :: (Board, [ColoredPiece])
initBoard = (board, pieces)
  where
    createMColoredPieces = map . (Just .: Create)
    board = [
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
    pieces = (map fromJust . filter isJust . concat) board



showBoard :: Board -> String
showBoard = unlines . (map . map) showMColoredPiece


printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

module Model where

import Data.List (partition, sort, group)
import GHC.Generics (Generic)

type Coord = (Int, Int)

boardSize :: Int
boardSize = 13

validCoord :: Coord -> Bool
validCoord (x, y)
    = inRange x && inRange y where
    inRange c = c >= 0 && c < boardSize

isCorner :: Coord -> Bool
isCorner (x, y)
    = x == 0 && y == 0
    || x == 0 && y == boardSize
    || x == boardSize && y == 0
    || x == boardSize && y == boardSize

isBorder :: Coord -> Bool
isBorder (x, y)
    = (x == 0 || y == 0 || x == boardSize || y == boardSize)
    && not (isCorner (x, y))

isCenter :: Coord -> Bool
isCenter (x, y)
    = p x && p y where
    p c = c >= 1 && c < boardSize - 1

distance :: Coord -> Coord -> Double
distance a b
    = sqrt (sq (x1 - x2) + sq (y1 - y2)) where
    (x1, y1, x2, y2)
        = ( fromIntegral (fst a)
          , fromIntegral (snd a)
          , fromIntegral (fst b)
          , fromIntegral (snd b)
          )
    sq x = x * x

type Board = [Piece]

data Piece
    = Piece
        { pieceCoord :: Coord
        , pieceSide :: Side
        }
    deriving (Eq)

data Side = Black | White
    deriving (Eq, Show, Generic)

emptyBoard :: Board
emptyBoard = []

sideFromInt :: Int -> Side
sideFromInt 0 = Black
sideFromInt 1 = White
sideFromInt _ = undefined

isOccupied :: Board -> Coord -> Bool
isOccupied board coord
    = any ((== coord) . pieceCoord) board

addPiece :: Board -> Side -> Coord -> Maybe Board
addPiece board side coord
    = if isOccupied board coord
        then Nothing
        else Just (Piece coord side : board)

won :: Board -> Side -> Coord -> Maybe [Coord]
won board side (coordX, coordY)
    = let
        hori = filter (\(Piece (x, _) sd) -> x == coordX && sd == side) board
        vert = filter (\(Piece (_, y) sd) -> y == coordY && sd == side) board
        crosslr = filter (\(Piece (x, y) sd) -> coordX - x == y - coordY && sd == side) board
        crossrl = filter (\(Piece (x, y) sd) -> coordX - x == coordY - y && sd == side) board
        isAdj (Piece c1 _) (Piece c2 _) = distance c1 c2 < 1.5
        group = foldl
            (\acc p ->
                let new = map (\g -> if any (isAdj p) g then p : g else g) acc
                in if any (any (== p)) new
                    then new
                    else [p] : new)
            []
        parts = group hori ++ group vert ++ group crosslr ++ group crossrl
        winning = filter ((> 4) . length) parts
    in case winning of
        part : _ -> Just (map (\(Piece coord _) -> coord) part)
        [] -> Nothing

flipSide :: Side -> Side
flipSide Black = White
flipSide White = Black

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both p q = (&&) <$> p <*> q

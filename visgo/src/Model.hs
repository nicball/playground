module Model where

import Data.List (partition, sort, group)
import GHC.Generics (Generic)

type Coord = (Int, Int)

boardSize :: Int
boardSize = 9

visionRange :: Double
visionRange = 1.5

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

type Board = [PieceGroup]

data PieceGroup
    = PieceGroup
        { members :: [Coord]
        , groupSide :: Side
        }

data Side = Black | White
    deriving (Eq, Show, Generic)

emptyBoard :: Board
emptyBoard = []

sideFromInt :: Int -> Side
sideFromInt 0 = Black
sideFromInt 1 = White
sideFromInt _ = undefined

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (x1, y1) (x2, y2)
    = sq (x1 - x2) + sq (y1 -y2) == 1 where
    sq x = x * x

isAdjToGroup :: Coord -> PieceGroup -> Bool
isAdjToGroup c pg
    = any (isAdjacent c) (members pg)

isVisibleToGroup:: Coord -> PieceGroup -> Bool
isVisibleToGroup c pg
    = any ((<= visionRange) . distance c) (members pg)

isVisibleToSide :: Board -> Side -> Coord -> Bool
isVisibleToSide pgs mySide coord
    = any
        (isVisibleToGroup coord)
        (filter ((== mySide) . groupSide) pgs)

adjacents :: Coord -> [Coord]
adjacents (x, y)
    = up ++ down ++ left ++ right where
    up = if y < boardSize - 1 then [(x, y + 1)] else []
    down = if y > 0 then [(x, y - 1)] else []
    left = if x > 1 then [(x - 1, y)] else []
    right = if x < boardSize - 1 then [(x + 1, y)] else []

isOccupied :: Board -> Coord -> Bool
isOccupied board coord
    = any (elem coord . members) board

groupLives :: Board -> PieceGroup -> Int
groupLives board (PieceGroup mems side)
    = length . group . sort
        . filter (not . isOccupied board)
        . concatMap adjacents
        $ mems

addPiece :: Board -> Side -> Coord -> Maybe Board
addPiece board side coord
    = if isOccupied board coord
        then
            (if not $ isVisibleToSide board side coord
                then Just board
                else Nothing)
        else
            if groupLives (newGroup : newRest) newGroup == 0
                then Nothing
                else Just (newGroup : newRest) where
    (adjs, rest)
        = partition
            (both (isAdjToGroup coord) ((== side) . groupSide))
            board
    newGroup
        = if null adjs
            then PieceGroup [coord] side
            else PieceGroup ([coord] ++ concatMap members adjs) side
    newRest = filter ((/= 0) . groupLives (newGroup : rest)) rest

visiblePieces :: Board -> Side -> [(Coord, Side)]
visiblePieces pgs mySide
    = concatMap f pgs where
    f (PieceGroup pieces side)
        = map
            (\c -> (c, side))
            (if side == mySide
                then pieces
                else filter (isVisibleToSide pgs mySide) pieces)

flipSide :: Side -> Side
flipSide Black = White
flipSide White = Black

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both p q = (&&) <$> p <*> q

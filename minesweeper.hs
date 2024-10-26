import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Row a = [a]
type Matrix a = [Row a]

staircase :: (Eq a, Num a) => Matrix a -> Matrix a
staircase = filter (any (/= 0)) . sortBy cmp where
  cmp (_:_) [] = error "cmp: unequal length"
  cmp [] (_:_) = error "cmp: unequal length"
  cmp (0 : xs) (0 : ys) = cmp xs ys
  cmp (0 : xs) _ = GT
  cmp _ (0 : ys) = LT
  cmp [] [] = EQ
  cmp (_:_) (_:_) = EQ

clearDown :: (Eq a, Fractional a) => Matrix a -> Matrix a
clearDown (r : rs) | all (== 0) r = r : rs
clearDown [] = []
clearDown (row : rest) = filter (any (/= 0)) $ row' : clearDown (staircase (map (clear row') rest)) where
  hd = head . filter (/= 0) $ row
  row' = map (/ hd) row
  clear (_:_) [] = error "clearDown: unequal length"
  clear [] (_:_) = error "clearDown: unequal length"
  clear (0 : xs) (0 : ys) = 0 : clear xs ys
  clear (0 : xs) _ = error "clearDown: not staircase "
  clear (x : xs) (0 : ys) = 0 : ys
  clear (x : xs) (y : ys) = 0 : zipWith (-) ys (map (\x' -> x' * y / x) xs)

clearUp :: (Eq a, Fractional a) => Matrix a -> Matrix a
clearUp = reverse . clearUp' . reverse where
  clearUp' (r : rs) | all (== 0) r = error "clearUp: zero row"
  clearUp' [] = []
  clearUp' (row : rest) = row : clearUp' (map (clear row) rest)
  clear (0 : xs) (y : ys) = y : clear xs ys
  clear (x : xs) (y : ys) = 0 : zipWith (-) ys (map (\x' -> x' * y / x) xs)
  clear [] _ = error "clearUp: zero row"
  clear _ [] = error "clearUp: unequal length"

gaussElim :: (Eq a, Fractional a) => Matrix a -> Matrix a
gaussElim = clearUp . clearDown . staircase

type Point = (Int, Int)
data Board a = Board
  { boardSize :: Int
  , boardNumbers :: Map Point (Maybe a)
  }
type Constraint a = ([Point], a)

neighbors :: Int -> Point -> [Point]
neighbors boardSize (x, y) = filter valid cands where
  cands =
    [ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    , (x - 1, y), (x + 1, y)
    , (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
    ]
  valid (x, y) = 0 <= x && x < boardSize && 0 <= y && y < boardSize

getConstraints :: Board a -> [Constraint a]
getConstraints (Board size numbers) = concatMap conv (Map.toList numbers) where
  conv (point, Just num) = [(filter (not . flip Map.member numbers) (neighbors size point), num)]
  conv (_, Nothing) = []

type VarTable = [Point]

allocateVars :: [Constraint a] -> VarTable
allocateVars = Set.toList . Set.fromList . concatMap fst

lookupPoint :: VarTable -> Point -> Int
lookupPoint vt t = go 0 vt where
  go n (p : ps) | p == t = n
  go n (_ : ps) = go (n + 1) ps
  go _ [] = error "lookupPoint: not found"

lookupVar :: VarTable -> Int -> Point
lookupVar vt n = vt !! n

constrToRow :: Num a => VarTable -> Constraint a -> Row a
constrToRow vt (ps, num) = [ if Set.member n vars then multiplicity n else 0 | n <- [ 0 .. numVars - 1 ] ] ++ [ num ] where
  vars = Set.fromList . map (lookupPoint vt) $ ps
  numVars = length vt
  multiplicity n = if even (fst p + snd p) then 1 else 2 where
    p = lookupVar vt n

rowMaximum :: (Ord a, Num a) => Row a -> a
rowMaximum = sum . filter (> 0) . init

rowMinimum :: (Ord a, Num a) => Row a -> a
rowMinimum = sum . filter (< 0) . init

data Info a = IsMine a | IsNum a deriving (Functor, Show, Eq, Ord)

solveRow :: (Ord a, Num a) => Row a -> [Info Int]
solveRow row
  = if last row == rowMaximum row
    then solveMax . init . zip [0 ..] $ row
    else if last row == rowMinimum row
      then solveMin . init . zip [0 ..] $ row
      else []
  where
    solveMax = concatMap $ \(i, n) ->
      if n > 0
        then [IsMine i]
        else if n < 0
          then [IsNum i]
          else []
    solveMin = concatMap $ \(i, n) ->
      if n > 0
        then [IsNum i]
        else if n < 0
          then [IsMine i]
          else []

translateInfo :: VarTable -> Info Int -> Info Point
translateInfo vt = fmap (lookupVar vt)

updateEquations :: (Num a) => [Info Int] -> Matrix a -> Matrix a
updateEquations info mat = map updateRow mat where
  updateRow row = coeff ++ [const] where
    coeff = [ if i `elem` indices then 0 else n | (i, n) <- init (zip [0 ..] row) ]
    indices = map getInfo info
    getInfo (IsMine a) = a
    getInfo (IsNum a) = a
    const = last row - sum
      [ case i of
          IsNum _ -> 0
          IsMine i -> row !! i
      | i <- info
      ]

solveBoard :: (Ord a, Fractional a) => Board a -> [Info Point]
solveBoard (Board size numbers) = map (translateInfo vt) (solve mat) where
  constrs = getConstraints (Board size numbers)
  vt = allocateVars constrs
  mat = gaussElim . map (constrToRow vt) $ constrs
  solve mat
    = if null info
        then []
        else info ++ solve (gaussElim . updateEquations info $ mat) where
    info = concatMap solveRow mat

boardFromScrshot :: (Eq a, Num a) => Int -> [[a]] -> Board a
boardFromScrshot size scrshot = Board size numbers where
  ps = concatMap (\(y, row) -> zipWith (\x n -> ((x, y), n)) [0 .. size - 1] row) . zip [size - 1, size - 2 .. 0] $ scrshot
  numbers = Map.fromList . concat $
    [ case n of
        -1 -> [(p, Nothing)]
        -2 -> []
        _ -> [(p, Just n)]
    | (p, n) <- ps
    ]

exampleMatrix =
  [ [ 1, 2, 3 ]
  , [ -2, 3, 0 ]
  , [ 0, 0, 1 ]
  , [ 0, 2, 3 ]
  , [ 0, 0, 7 ]
  , [ 0, 1, 0 ]
  ]

test_staircase = staircase exampleMatrix
test_clearDown = clearDown test_staircase
test_clearUp = clearUp test_clearDown

exampleBoard = boardFromScrshot 5 $
  [ [ o, o, o, o, o ]
  , [ o, o, o, o, o ]
  , [ o, o, 4, o, o ]
  , [ o, o, o, o, 2 ]
  , [ o, o, o, 4, o ]
  ] where
  o = -2 -- unknown
  x = -1 -- secret number

printMat :: Show a => Matrix a -> IO ()
printMat = mapM_ print

main = do
  -- print (Map.toList (boardNumbers exampleBoard))
  let
    constrs = getConstraints exampleBoard
    vt = allocateVars constrs
    mat = gaussElim . map (constrToRow vt) $ constrs
  print vt
  printMat (map (constrToRow vt) constrs)
  -- printMat (staircase (map (constrToRow vt) constrs))
  printMat mat
  let
    info = concatMap solveRow mat
    mat' = gaussElim . updateEquations info $ mat
  print (map (translateInfo vt) info)
  -- printMat mat'
  -- print (map (translateInfo vt) (concatMap solveRow mat'))
  -- print (concatMap solveRow mat')
  print (solveBoard exampleBoard)

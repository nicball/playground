module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Data.Ratio

type Freq a = (a, Rational)
data Gene = Dominant Char | Recessive Char
data Allele = GAA Char | GAa Char | Gaa Char deriving (Eq, Ord)
type Chromo = [Allele]
type Gamete = [Gene]
type Population = [Freq Chromo]

geneChar :: Gene -> Char
geneChar g = 
    case g of
        (Dominant c)  -> c
        (Recessive c) -> c
        
instance Eq Gene where
    x == y = (geneChar x) == (geneChar y)

segregateGene :: Allele -> [Freq Gene]
segregateGene (GAA c) = [((Dominant c), 1.0)]
segregateGene (GAa c) = [((Dominant c), 0.5), ((Recessive c), 0.5)]
segregateGene (Gaa c) = [((Recessive c), 1.0)]

combineGene :: Gene -> Gene -> Allele
combineGene x y =
    if geneChar x /= geneChar y
        then error "combining two different genes"
        else case (x, y) of
            ((Dominant c), (Dominant _))   -> GAA c
            ((Dominant c), (Recessive _))  -> GAa c
            ((Recessive c), (Dominant _))  -> GAa c
            ((Recessive c), (Recessive _)) -> Gaa c

freeCombine :: [[Freq Gene]] -> [Freq Gamete]
freeCombine []     = [([], 1%1)]
freeCombine (x:xs) = let rest = freeCombine xs
                     in [(x':y', f1*f2) | (x',f1) <- x, (y',f2) <- rest]

meiosis :: Chromo -> [Freq Gamete]
meiosis = freeCombine . map segregateGene

freeCopulate :: [[Freq Gamete]] -> Population
freeCopulate []     = []
freeCopulate (x:xs) = concat [[coitus x' y' | x' <- x, y' <- y] | y <- xs]
                   ++ [selfCoitus x' x'' | x' <- x, x'' <- x]
                   ++ freeCopulate xs
     where coitus (xs, f1) (ys, f2) = (zipWith combineGene xs ys, f1*f2*2)
           selfCoitus x@(xs, f1) y@(ys, f2) = if xs == ys
               then (zipWith combineGene xs xs, f1*f1)
               else coitus x y

populate :: Population -> Population
populate = clear . disjoin . freeCopulate . produceGametes
    where produceGametes = map (\(ch, f1) -> map (\(g, f2) -> (g, (f1*f2))) (meiosis ch))
          disjoin = map (foldr (\(x, f1) (_, f2) -> (x, f1+f2)) ([], 0%1)) . groupBy (\(x, _) (y, _) -> x == y) . sort
          clear = filter ((/= 0%1) . snd)

main :: IO ()
main = printSteps 15
    [([(GAA 'a'), (GAa 'b')], 4%10),
     ([(Gaa 'a'), (GAa 'b')], 6%10)]

printSteps :: Int -> Population -> IO ()
printSteps 0 _ = return ()
printSteps n p = do
    printPopulation p
    putStrLn ""
    printSteps (n-1) (populate p)

printPopulation :: Population -> IO ()
printPopulation p = forM_ p $ \(ch, f) -> do
    printChromo ch
    putStr $ "(" ++ show (((fromRational f) :: Double) * 100.0) ++ "%) "

printChromo :: Chromo -> IO ()
printChromo ch = forM_ ch $ \al ->
    case al of
        (GAA c) -> putStr [(toUpper c), (toUpper c)]
        (GAa c) -> putStr [(toUpper c), (toLower c)]
        (Gaa c) -> putStr [(toLower c), (toLower c)]

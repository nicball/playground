module Main where

import Control.Arrow
import Control.Monad
-- import Control.Parallel.Strategies
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import System.Random
-- import Text.Printf (printf)

type Term = String
type Phrase = (Term, Term)
type Dict = M.Map Phrase (M.Map Term Double)

isChinese :: Char -> Bool
isChinese c = isLetter c && not (isEnglish c)

isEnglish :: Char -> Bool
isEnglish c = isUpper c || isLower c || c == '_'

isPunct :: Char -> Bool
isPunct x = isPunctuation x && x /= '_'

trimL :: String -> String
trimL = dropWhile isSpace

concatTerm :: Term -> Term -> String
concatTerm a b =
    if isEnglish (head a)
    then a ++ " " ++ b
    else a ++ b

splitTerms :: String -> [Term]
splitTerms str =
    go (trimL . filter isValidChar $ str)
  where
    go s@(c:cs) =
        if isChinese c || isPunct c
        then [c] : go (trimL cs)
        else let (w, r) = span isEnglish s
              in w : go (trimL r)
    go [] = []
    isValidChar x = isChinese x || isEnglish x || isPunct x || isSpace x

postfixFreq :: [(Phrase, Term)] -> M.Map Term Double
postfixFreq ps | not (null ps) =
    let plength = fromIntegral . length
        count lst = (snd (head lst), plength lst / plength ps)
    in M.fromList . map count . group $ ps

learn :: [(Phrase, Term)] -> Dict
learn = toDict . groupBy ((==) `on` fst) . sort
    where toDict = M.fromList . map toEntry
          toEntry ws = (fst (head ws), postfixFreq ws)

startOfLine :: Term
startOfLine = ['燊']

endOfLine :: Term
endOfLine = ['龘']

jibber :: Phrase -> Dict -> IO ()
jibber ph dict = do
    let lastTerm = snd ph
    putStr lastTerm
    when (isEnglish (head lastTerm)) (putChar ' ')
    case M.lookup ph dict of
        Just nexts -> do
            (n, _) <- randNth (M.toList nexts)
            when (n /= endOfLine) (jibber (lastTerm, n) dict)
        Nothing -> return ()

preprocess :: String -> String
preprocess = unlines . map (startOfLine ++) . map (++ endOfLine) . lines

main :: IO ()
main = do
    article <- getContents
    when (not (null article)) $ do
        let terms = splitTerms (preprocess article)
            couples = map merge $ zip3 terms (tail terms) (tail (tail terms))
            merge (a, b, c) = ((a, b), c)
            dict = learn couples
        -- putStrLn . show $ dict
        (fstPhrase, _) <- randNth . filter isStarter . M.toList $ dict
        jibber fstPhrase dict
    where isStarter ((a, _), _) = a == startOfLine

randNth :: [a] -> IO a
randNth lst = do
    gen <- getStdGen
    let (i, gen') = next gen
    setStdGen gen'
    return $ lst !! (i `mod` length lst)

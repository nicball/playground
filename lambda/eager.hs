module Main where

data Expr
    = Var Int
    | Abs String Expr
    | App Expr Expr
    | Zero
    | Succ Expr
    | Rec Expr Expr String String Expr

shift :: Int -> Expr -> Expr
shift n e = go n 0 e where
    go n l (Var i) = Var (if i >= l then i + n else i)
    go n l (Abs name body) = Abs name (go n (l + 1) body)
    go n l (App f x) = App (go n l f) (go n l x)
    go n l Zero = Zero
    go n l (Succ e) = Succ (go n l e)
    go n l (Rec t z e p s) = Rec (go n l t) (go n l z) e p (go n (l + 2) s)

instance Show Expr where
    show = go 0 [] where
        go indent names (Var i) = lookup [] names i where
            lookup _ [] i = show i
            lookup inner (n : ns) 0 = n ++ if n `elem` inner then "_" ++ show i else ""
            lookup inner (n : ns) i = lookup (n : inner) ns (i - 1)
        go indent names (Abs name body) = "(" ++ name ++ " =>\n" ++
            showIndent (indent + 1) ++ go (indent + 1) (name : names) body ++ "\n" ++
            showIndent indent ++ ")"
        go indent names (App f x) = "(" ++ go indent names f ++ " " ++ go indent names x ++ ")"
        go indent names Zero = "0"
        go indent names e@(Succ _) =
            case countSuccs e of
                (n, Zero) -> show n
                (n, e) -> show n ++ " + " ++ go indent names e
            where
                countSuccs (Succ e) = (n + 1, rest) where
                    (n, rest) = countSuccs e
                countSuccs x = (0, x)
        go indent names (Rec t z e p s) = "(recurse on " ++ go (indent + 1) names t ++ "\n" ++
            showIndent (indent + 1) ++ "when 0 -> " ++ go (indent + 1) names z ++ "\n" ++
            showIndent (indent + 1) ++ "when succ " ++ e ++ " with " ++ p ++ " -> " ++ go (indent + 1) (p : e : names) s ++ "\n" ++
            showIndent indent ++ ")"

        showIndent n = concat . replicate n $ "  "

-- cps :: Expr -> Expr
-- cps e@(Var _) = Abs "cont" (App (Var 0) (shift 1 e))
-- cps (Abs name body) = Abs name (cps body)
-- cps (App f x) =
--     let f' = cps f
--         x' = cps x
--     in App f' (Abs "f'" (App (shift 1 x') (Abs "x'" (App (Var 1) (Var 0)))))

isValue :: Expr -> Bool
isValue (Abs _ body) = isValue body
isValue Zero = True
isValue (Succ e) = isValue e
isValue x = isNeutral x

isNeutral :: Expr -> Bool
isNeutral (Var _) = True
isNeutral (App f x) = isNeutral f && isValue x
isNeutral (Rec t z _ _ s) = isNeutral t && isValue z && isValue s
isNeutral _ = False

type Substituter = Int -> Expr

shiftSub :: Substituter -> Substituter
shiftSub s 0 = Var 0
shiftSub s n = shift 1 (s (n - 1))

substitute :: Substituter -> Expr -> Expr
substitute s (Var n) = s n
substitute s (Abs name body) = Abs name (substitute (shiftSub s) body)
substitute s (App f x) = App (substitute s f) (substitute s x)
substitute s Zero = Zero
substitute s (Succ n) = Succ (substitute s n)
substitute s (Rec t z r p succ) = Rec (substitute s t) (substitute s z) r p (substitute (shiftSub (shiftSub s)) succ)

infixl <.>
(<.>) :: Expr -> Expr -> Expr
f <.> x = substitute s f where
    s 0 = x
    s n = Var (n - 1)

step :: Expr -> Expr
step (Abs name body) | not (isValue body) = Abs name (step body)
step (App (Abs _ body) x) = body <.> x
step (App f x) | not (isNeutral f) = App (step f) x
step (App f x) | not (isValue x) = App f (step x)
step (Succ e) | not (isValue e) = Succ (step e)
step (Rec Zero z _ _ _) = z
step (Rec (Succ e) z r p s) = s <.> Rec e z r p s <.> e
step (Rec t z e p s) | not (isNeutral t) = Rec (step t) z e p s
step (Rec t z e p s) | not (isValue z) = Rec t (step z) e p s
step (Rec t z e p s) | not (isValue s) = Rec t z e p (step s)
step _ = error "stuck"

instance Eq Expr where
    (==) (Var i) (Var j) = i == j
    (==) (Abs _ body1) (Abs _ body2) = body1 == body2
    (==) (App f1 x1) (App f2 x2) = f1 == f2 && x1 == x2
    (==) Zero Zero = True
    (==) (Succ e1) (Succ e2) = e1 == e2
    (==) (Rec t1 z1 _ _ s1) (Rec t2 z2 _ _ s2) = t1 == t2 && z1 == z2 && s1 == s2
    (==) _ _ = False

trace :: Expr -> IO ()
trace e = do
    print e
    if isValue e
        then pure ()
        else trace (step e)

-- yc f = f (yc f)
-- yc = \f. (\x. f (x x)) (\x. f (x x))
yc :: Expr
yc = Abs "f" (App (Abs "x" (App (Var 1) (App (Var 0) (Var 0)))) (Abs "x" (App (Var 1) (App (Var 0) (Var 0)))))

add :: Expr
add = Abs "x" (Abs "y" (Rec (Var 0) (Var 1) "e" "p" (Succ (Var 0))))

mul :: Expr
mul = Abs "add" (Abs "x" (Abs "y" (Rec (Var 0) Zero "e" "p" (App (App (Var 4) (Var 0)) (Var 3)))))

five :: Expr
five = Succ (Succ (Succ (Succ (Succ Zero))))

main :: IO ()
main = print yc
    >> print five
    >> trace (App (App add Zero) five)
    >> trace (App (App (App mul add) five) five)

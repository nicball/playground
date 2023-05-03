module Main where

data Expr
  = Var Int
  | Abs String Expr
  | App Expr Expr
  | Zero
  | Succ Expr
  | Rec Expr Expr String String Expr
  | Thunk Symbol
  | ForceNat Int Expr

shift :: Int -> Expr -> Expr
shift n e = go n 0 e where
  go n l (Var i) = Var (if i >= l then i + n else i)
  go n l (Abs name body) = Abs name (go n (l + 1) body)
  go n l (App f x) = App (go n l f) (go n l x)
  go n l Zero = Zero
  go n l (Succ e) = Succ (go n l e)
  go n l (Rec t oz r p os) = Rec (go n l t) (go n l oz) r p (go n (l + 2) os)
  go n l (Thunk t) = Thunk t
  go n l (ForceNat acc e) = ForceNat acc (go n l e)

countSuccs :: Expr -> (Int, Expr)
countSuccs (Succ e) = (n + 1, rest) where
  (n, rest) = countSuccs e
countSuccs x = (0, x)

showExprWithIndent :: Int -> Expr -> String
showExprWithIndent n = go n [] where
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
      (n, e) -> "(" ++ show n ++ " + " ++ go (indent + 1) names e ++ ")"
  go indent names (Rec t oz r p os) = "(recurse on " ++ go (indent + 1) names t ++ "\n" ++
    showIndent (indent + 1) ++ "when 0 -> " ++ go (indent + 1) names oz ++ "\n" ++
    showIndent (indent + 1) ++ "when succ " ++ r ++ " with " ++ p ++ " -> " ++ go (indent + 1) (p : r : names) os ++ "\n" ++
    showIndent indent ++ ")"
  go _ _ (Thunk (Symbol t)) = "@" ++ show t
  go indent names (ForceNat acc e) = "(force-nat " ++ show acc ++ "\n" ++
    showIndent (indent + 1) ++ go (indent + 1) names e ++
    showIndent indent ++ ")"

  showIndent n = concat . replicate n $ "  "

instance Show Expr where
  show = showExprWithIndent 0

-- cps :: Expr -> Expr
-- cps e@(Var _) = Abs "cont" (App (Var 0) (shift 1 e))
-- cps (Abs name body) = Abs name (cps body)
-- cps (App f x) =
--   let f' = cps f
--     x' = cps x
--   in App f' (Abs "f'" (App (shift 1 x') (Abs "x'" (App (Var 1) (Var 0)))))

isValue :: Expr -> Bool
isValue (Abs _ _) = True
isValue Zero = True
isValue (Succ (Thunk _)) = True
isValue (ForceNat _ Zero) = True
isValue x = isNeutral x

isNeutral :: Expr -> Bool
isNeutral (Var _) = True
isNeutral (App f x) = isNeutral f -- && isValue x
isNeutral (Rec t oz _ _ os) = isNeutral t -- && isValue oz && isValue os
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
substitute s (Rec t oz r p os) = Rec (substitute s t) (substitute s oz) r p (substitute (shiftSub (shiftSub s)) os)
substitute _ e@(Thunk _) = e
substitute s (ForceNat acc e) = ForceNat acc (substitute s e)

infixl </>
(</>) :: Expr -> Expr -> Expr
f </> x = substitute s f where
  s 0 = x
  s n = Var (n - 1)

data Store = Store Int [(Symbol, Maybe Expr)]
data Symbol = Symbol Int
  deriving Eq

instance Show Store where
  show (Store _ xs) = "{" ++ go [] xs ++ "\n}" where
    go seen ((Symbol n, cell) : xs)
      | n `notElem` seen = "\n  @" ++ show n ++ " = " ++ showCell cell ++ ";" ++ go (n : seen) xs
      | otherwise = go seen xs
    go _ [] = ""
    showCell Nothing = "●"
    showCell (Just e) = showExprWithIndent 2 e

empty :: Store
empty = Store 0 []

fresh :: Store -> (Expr -> (Expr, a)) -> (Store, a)
fresh (Store n xs) f = (Store (n + 1) ((Symbol n, Just e) : xs), r) where
  (e, r) = f (Thunk (Symbol n))

lookupStore :: Store -> Symbol -> (Store, Expr)
lookupStore (Store n all) a = go all where
  go ((b, Just e) : xs) | a == b && not (isValue e) = (Store n' ((a, Just e') : all'), Thunk a) where
    (Store n' all', e') = step (Store n ((a, Nothing) : all)) e
  go ((b, Just e) : xs) | a == b = (Store n all, e) where
  go ((b, Nothing) : _) | a == b = error "infinite recursion"
  go (_ : s) = go s
  go [] = error "impossible"

isThunk :: Expr -> Bool
isThunk (Thunk _) = True
isThunk _ = False

step :: Store -> Expr -> (Store, Expr)
step s (App (Abs _ body) x) = fresh s $ \x' -> (x, body </> x')
step s (App f x) | not (isNeutral f) = (s', App f' x) where
  (s', f') = step s f
step s (App f x) | not (isValue x) = (s', App f x') where
  (s', x') = step s x
step s (Succ e) | not (isThunk e) = fresh s $ \e' -> (e, Succ e')
step s (Rec Zero oz _ _ _) = (s, oz)
step s (Rec (Succ e) oz r p os) | isThunk e = fresh s $ \partial -> (Rec e oz r p os, os </> partial </> e)
step s (Rec t oz r p os) | not (isNeutral t) = (s', Rec t' oz r p os) where
  (s', t') = step s t
step s (Thunk a) = lookupStore s a
step s (ForceNat n (Succ e)) = (s, ForceNat (n + 1) e)
step s (ForceNat n e) | not (isValue e) = (s', ForceNat n e') where
  (s', e') = step s e
step s e = error ("stuck on " ++ show e)

-- instance Eq Expr where
--   (==) (Var i) (Var j) = i == j
--   (==) (Abs _ body1) (Abs _ body2) = body1 == body2
--   (==) (App f1 x1) (App f2 x2) = f1 == f2 && x1 == x2
--   (==) Zero Zero = True
--   (==) (Succ e1) (Succ e2) = e1 == e2
--   (==) (Rec t1 z1 _ _ s1) (Rec t2 z2 _ _ s2) = t1 == t2 && z1 == z2 && s1 == s2
--   (==) _ _ = False

trace :: Expr -> IO ()
trace e = go empty e where
  go s e = do
    print e
    print s
    putChar '\n'
    if isValue e
      then pure ()
      else let (s', e') = step s e in go s' e'

-- yc f = f (yc f)
-- yc = \f. (\x. f (x x)) (\x. f (x x))
yc :: Expr
yc = Abs "f" (App (Abs "x" (App (Var 1) (App (Var 0) (Var 0)))) (Abs "x" (App (Var 1) (App (Var 0) (Var 0)))))

add :: Expr
add = Abs "x" (Abs "y" (Rec (Var 0) (Var 1) "e" "p" (Succ (Var 0))))

mul :: Expr
mul = Abs "add" (Abs "x" (Abs "y" (Rec (Var 0) Zero "e" "p" (App (App (Var 4) (Var 0)) (Var 3)))))

three :: Expr
three = Succ (Succ (Succ Zero))

main :: IO ()
main = print yc
  >> print three
  >> trace (ForceNat 0 (App (App add Zero) three))
  >> trace (ForceNat 0 (App (App (App mul add) three) three))
  >> trace (Abs "_" (App (Abs "x" (App (Var 0) (Var 0))) (Abs "x" (App (Var 0) (Var 0)))))

{-

(f =>
  ((x =>
    (f (x x))
  ) (x =>
    (f (x x))
  ))
)
3
(force-nat 0
  (((x =>
    (y =>
      (recurse on y
        when 0 -> x
        when succ e with p -> (1 + p)
      )
    )
  ) 0) 3))
{
}

(force-nat 0
  ((y =>
    (recurse on y
      when 0 -> @0
      when succ e with p -> (1 + p)
    )
  ) 3))
{
  @0 = 0;
}

(force-nat 0
  (recurse on @1
    when 0 -> @0
    when succ e with p -> (1 + p)
  ))
{
  @1 = 3;
  @0 = 0;
}

(force-nat 0
  (recurse on @1
    when 0 -> @0
    when succ e with p -> (1 + p)
  ))
{
  @1 = (1 + @2);
  @2 = 2;
  @0 = 0;
}

(force-nat 0
  (recurse on (1 + @2)
    when 0 -> @0
    when succ e with p -> (1 + p)
  ))
{
  @1 = (1 + @2);
  @2 = 2;
  @0 = 0;
}

(force-nat 0
  (1 + @3))
{
  @3 = (recurse on @2
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @1 = (1 + @2);
  @2 = 2;
  @0 = 0;
}

(force-nat 1
  @3)
{
  @3 = (recurse on @2
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @1 = (1 + @2);
  @2 = 2;
  @0 = 0;
}

(force-nat 1
  @3)
{
  @3 = (recurse on @2
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @2 = (1 + @4);
  @4 = 1;
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 1
  @3)
{
  @3 = (recurse on (1 + @4)
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @2 = (1 + @4);
  @4 = 1;
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 1
  @3)
{
  @3 = (1 + @5);
  @5 = (recurse on @4
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @2 = (1 + @4);
  @4 = 1;
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 1
  (1 + @5))
{
  @3 = (1 + @5);
  @5 = (recurse on @4
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @2 = (1 + @4);
  @4 = 1;
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 2
  @5)
{
  @3 = (1 + @5);
  @5 = (recurse on @4
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @2 = (1 + @4);
  @4 = 1;
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 2
  @5)
{
  @5 = (recurse on @4
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 2
  @5)
{
  @5 = (recurse on (1 + @6)
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 2
  @5)
{
  @5 = (1 + @7);
  @7 = (recurse on @6
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 2
  (1 + @7))
{
  @5 = (1 + @7);
  @7 = (recurse on @6
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 3
  @7)
{
  @5 = (1 + @7);
  @7 = (recurse on @6
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 3
  @7)
{
  @7 = (recurse on 0
      when 0 -> @0
      when succ e with p -> (1 + p)
    );
  @5 = (1 + @7);
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 3
  @7)
{
  @7 = @0;
  @5 = (1 + @7);
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 3
  @7)
{
  @7 = 0;
  @5 = (1 + @7);
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 3
  0)
{
  @7 = 0;
  @5 = (1 + @7);
  @4 = (1 + @6);
  @6 = 0;
  @3 = (1 + @5);
  @2 = (1 + @4);
  @1 = (1 + @2);
  @0 = 0;
}

(force-nat 0
  ((((add =>
    (x =>
      (y =>
        (recurse on y
          when 0 -> 0
          when succ e with p -> ((add p) x)
        )
      )
    )
  ) (x =>
    (y =>
      (recurse on y
        when 0 -> x
        when succ e with p -> (1 + p)
      )
    )
  )) 3) 3))
{
}

(force-nat 0
  (((x =>
    (y =>
      (recurse on y
        when 0 -> 0
        when succ e with p -> ((@0 p) x)
      )
    )
  ) 3) 3))
{
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  ((y =>
    (recurse on y
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    )
  ) 3))
{
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on @2
    when 0 -> 0
    when succ e with p -> ((@0 p) @1)
  ))
{
  @2 = 3;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on @2
    when 0 -> 0
    when succ e with p -> ((@0 p) @1)
  ))
{
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on (1 + @3)
    when 0 -> 0
    when succ e with p -> ((@0 p) @1)
  ))
{
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  ((@0 @4) @1))
{
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (((x =>
    (y =>
      (recurse on y
        when 0 -> x
        when succ e with p -> (1 + p)
      )
    )
  ) @4) @1))
{
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  ((y =>
    (recurse on y
      when 0 -> @5
      when succ e with p -> (1 + p)
    )
  ) @1))
{
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on @6
    when 0 -> @5
    when succ e with p -> (1 + p)
  ))
{
  @6 = @1;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @1 = 3;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on @6
    when 0 -> @5
    when succ e with p -> (1 + p)
  ))
{
  @6 = @1;
  @1 = (1 + @7);
  @7 = 2;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on @6
    when 0 -> @5
    when succ e with p -> (1 + p)
  ))
{
  @6 = (1 + @7);
  @1 = (1 + @7);
  @7 = 2;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (recurse on (1 + @7)
    when 0 -> @5
    when succ e with p -> (1 + p)
  ))
{
  @6 = (1 + @7);
  @1 = (1 + @7);
  @7 = 2;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 0
  (1 + @8))
{
  @8 = (recurse on @7
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @6 = (1 + @7);
  @1 = (1 + @7);
  @7 = 2;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 1
  @8)
{
  @8 = (recurse on @7
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @6 = (1 + @7);
  @1 = (1 + @7);
  @7 = 2;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 1
  @8)
{
  @8 = (recurse on @7
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @7 = (1 + @9);
  @9 = 1;
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 1
  @8)
{
  @8 = (recurse on (1 + @9)
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @7 = (1 + @9);
  @9 = 1;
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 1
  @8)
{
  @8 = (1 + @10);
  @10 = (recurse on @9
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @7 = (1 + @9);
  @9 = 1;
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 1
  (1 + @10))
{
  @8 = (1 + @10);
  @10 = (recurse on @9
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @7 = (1 + @9);
  @9 = 1;
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 2
  @10)
{
  @8 = (1 + @10);
  @10 = (recurse on @9
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @7 = (1 + @9);
  @9 = 1;
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 2
  @10)
{
  @10 = (recurse on @9
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 2
  @10)
{
  @10 = (recurse on (1 + @11)
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 2
  @10)
{
  @10 = (1 + @12);
  @12 = (recurse on @11
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 2
  (1 + @12))
{
  @10 = (1 + @12);
  @12 = (recurse on @11
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @10 = (1 + @12);
  @12 = (recurse on @11
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = (recurse on 0
      when 0 -> @5
      when succ e with p -> (1 + p)
    );
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @2 = (1 + @3);
  @3 = 2;
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (recurse on @3
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (recurse on (1 + @13)
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = ((@0 @14) @1);
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (((x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    ) @14) @1);
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = ((y =>
      (recurse on y
        when 0 -> @15
        when succ e with p -> (1 + p)
      )
    ) @1);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (recurse on @16
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = @1;
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (recurse on @16
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (recurse on (1 + @7)
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = @4;
  @4 = (1 + @17);
  @17 = (recurse on @7
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = @5;
  @5 = (1 + @17);
  @4 = (1 + @17);
  @17 = (recurse on @7
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  @12)
{
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @17 = (recurse on @7
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 3
  (1 + @17))
{
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @17 = (recurse on @7
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 4
  @17)
{
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @17 = (recurse on @7
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 4
  @17)
{
  @17 = (recurse on (1 + @9)
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 4
  @17)
{
  @17 = (1 + @18);
  @18 = (recurse on @9
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 4
  (1 + @18))
{
  @17 = (1 + @18);
  @18 = (recurse on @9
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 5
  @18)
{
  @17 = (1 + @18);
  @18 = (recurse on @9
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 5
  @18)
{
  @18 = (recurse on (1 + @11)
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 5
  @18)
{
  @18 = (1 + @19);
  @19 = (recurse on @11
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 5
  (1 + @19))
{
  @18 = (1 + @19);
  @19 = (recurse on @11
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @18 = (1 + @19);
  @19 = (recurse on @11
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = (recurse on 0
      when 0 -> @15
      when succ e with p -> (1 + p)
    );
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @3 = (1 + @13);
  @13 = 1;
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (recurse on @13
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (recurse on (1 + @20)
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = ((@0 @21) @1);
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (((x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    ) @21) @1);
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = ((y =>
      (recurse on y
        when 0 -> @22
        when succ e with p -> (1 + p)
      )
    ) @1);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (recurse on @23
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = @1;
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (recurse on @23
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (recurse on (1 + @7)
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = @14;
  @14 = (1 + @24);
  @24 = (recurse on @7
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = @15;
  @15 = (1 + @24);
  @14 = (1 + @24);
  @24 = (recurse on @7
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  @19)
{
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @24 = (recurse on @7
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 6
  (1 + @24))
{
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @24 = (recurse on @7
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 7
  @24)
{
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @24 = (recurse on @7
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 7
  @24)
{
  @24 = (recurse on (1 + @9)
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 7
  @24)
{
  @24 = (1 + @25);
  @25 = (recurse on @9
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 7
  (1 + @25))
{
  @24 = (1 + @25);
  @25 = (recurse on @9
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 8
  @25)
{
  @24 = (1 + @25);
  @25 = (recurse on @9
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 8
  @25)
{
  @25 = (recurse on (1 + @11)
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 8
  @25)
{
  @25 = (1 + @26);
  @26 = (recurse on @11
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 8
  (1 + @26))
{
  @25 = (1 + @26);
  @26 = (recurse on @11
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @25 = (1 + @26);
  @26 = (recurse on @11
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = (recurse on 0
      when 0 -> @22
      when succ e with p -> (1 + p)
    );
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = @22;
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @22 = @21;
  @21 = (recurse on @20
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = @22;
  @22 = @21;
  @21 = (recurse on 0
      when 0 -> 0
      when succ e with p -> ((@0 p) @1)
    );
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = @22;
  @22 = @21;
  @21 = 0;
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = @22;
  @22 = 0;
  @21 = 0;
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  @26)
{
  @26 = 0;
  @22 = 0;
  @21 = 0;
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(force-nat 9
  0)
{
  @26 = 0;
  @22 = 0;
  @21 = 0;
  @25 = (1 + @26);
  @24 = (1 + @25);
  @19 = (1 + @24);
  @15 = (1 + @24);
  @14 = (1 + @24);
  @23 = (1 + @7);
  @13 = (1 + @20);
  @20 = 0;
  @18 = (1 + @19);
  @17 = (1 + @18);
  @12 = (1 + @17);
  @5 = (1 + @17);
  @4 = (1 + @17);
  @16 = (1 + @7);
  @3 = (1 + @13);
  @10 = (1 + @12);
  @9 = (1 + @11);
  @11 = 0;
  @8 = (1 + @10);
  @7 = (1 + @9);
  @6 = (1 + @7);
  @1 = (1 + @7);
  @2 = (1 + @3);
  @0 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ e with p -> (1 + p)
        )
      )
    );
}

(_ =>
  ((x =>
    (x x)
  ) (x =>
    (x x)
  ))
)
{
}

-}

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

recurseWithY :: Expr
recurseWithY = App yc (Abs "add" (Abs "x" (Abs "y" (Rec (Var 0) (Var 1) "y-1" "_" (Succ (App (App (Var 4) (Var 3)) (Var 1)))))))

main :: IO ()
main
  -- = print yc
  -- >> print three
  -- >> trace (ForceNat 0 (App (App add Zero) three))
  -- >> trace (ForceNat 0 (App (App (App mul add) three) three))
  -- >> trace (Abs "_" (App (Abs "x" (App (Var 0) (Var 0))) (Abs "x" (App (Var 0) (Var 0)))))
  = trace (ForceNat 0 (App (App recurseWithY (Succ Zero)) (Succ Zero)))

{-

(force-nat 0
  ((((f =>
    ((x =>
      (f (x x))
    ) (x =>
      (f (x x))
    ))
  ) (add =>
    (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((add x) y-1))
        )
      )
    )
  )) 1) 1))
{
}

(force-nat 0
  ((((x =>
    (@0 (x x))
  ) (x =>
    (@0 (x x))
  )) 1) 1))
{
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (((@0 (@1 @1)) 1) 1))
{
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  ((((add =>
    (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((add x) y-1))
        )
      )
    )
  ) (@1 @1)) 1) 1))
{
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (((x =>
    (y =>
      (recurse on y
        when 0 -> x
        when succ y-1 with _ -> (1 + ((@2 x) y-1))
      )
    )
  ) 1) 1))
{
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  ((y =>
    (recurse on y
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    )
  ) 1))
{
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (recurse on @4
    when 0 -> @3
    when succ y-1 with _ -> (1 + ((@2 @3) y-1))
  ))
{
  @4 = 1;
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (recurse on @4
    when 0 -> @3
    when succ y-1 with _ -> (1 + ((@2 @3) y-1))
  ))
{
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (recurse on (1 + @5)
    when 0 -> @3
    when succ y-1 with _ -> (1 + ((@2 @3) y-1))
  ))
{
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 0
  (1 + ((@2 @3) @5)))
{
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((@2 @3) @5))
{
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @2 = (@1 @1);
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((@2 @3) @5))
{
  @2 = ((x =>
      (@0 (x x))
    ) @1);
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((@2 @3) @5))
{
  @2 = (@0 (@7 @7));
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((@2 @3) @5))
{
  @2 = ((add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    ) (@7 @7));
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((@2 @3) @5))
{
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  (((x =>
    (y =>
      (recurse on y
        when 0 -> x
        when succ y-1 with _ -> (1 + ((@8 x) y-1))
      )
    )
  ) @3) @5))
{
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  ((y =>
    (recurse on y
      when 0 -> @9
      when succ y-1 with _ -> (1 + ((@8 @9) y-1))
    )
  ) @5))
{
  @9 = @3;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  (recurse on @10
    when 0 -> @9
    when succ y-1 with _ -> (1 + ((@8 @9) y-1))
  ))
{
  @10 = @5;
  @9 = @3;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  (recurse on @10
    when 0 -> @9
    when succ y-1 with _ -> (1 + ((@8 @9) y-1))
  ))
{
  @10 = 0;
  @9 = @3;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  (recurse on 0
    when 0 -> @9
    when succ y-1 with _ -> (1 + ((@8 @9) y-1))
  ))
{
  @10 = 0;
  @9 = @3;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  @9)
{
  @10 = 0;
  @9 = @3;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @3 = 1;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  @9)
{
  @9 = @3;
  @3 = (1 + @11);
  @11 = 0;
  @10 = 0;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  @9)
{
  @9 = (1 + @11);
  @3 = (1 + @11);
  @11 = 0;
  @10 = 0;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 1
  (1 + @11))
{
  @9 = (1 + @11);
  @3 = (1 + @11);
  @11 = 0;
  @10 = 0;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 2
  @11)
{
  @9 = (1 + @11);
  @3 = (1 + @11);
  @11 = 0;
  @10 = 0;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

(force-nat 2
  0)
{
  @9 = (1 + @11);
  @3 = (1 + @11);
  @11 = 0;
  @10 = 0;
  @2 = (x =>
      (y =>
        (recurse on y
          when 0 -> x
          when succ y-1 with _ -> (1 + ((@8 x) y-1))
        )
      )
    );
  @8 = (@7 @7);
  @7 = @1;
  @6 = (recurse on @5
      when 0 -> @3
      when succ y-1 with _ -> (1 + ((@2 @3) y-1))
    );
  @4 = (1 + @5);
  @5 = 0;
  @1 = (x =>
      (@0 (x x))
    );
  @0 = (add =>
      (x =>
        (y =>
          (recurse on y
            when 0 -> x
            when succ y-1 with _ -> (1 + ((add x) y-1))
          )
        )
      )
    );
}

-}

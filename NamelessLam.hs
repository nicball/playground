module Main where

import Control.Applicative
import Control.Monad.Fix

data Term
  = Number Int
  | Boolean Bool
  | Ident Int
  | Abstr Term
  | App Term Term
  | AppPrim Prim [Term]
  deriving (Eq)

data Prim
  = If
  | Equal
  | Minus
  | Multi
  deriving (Show, Eq)

fromAbstr :: Term -> Term
fromAbstr (Abstr t) = t

instance Show Term where
  show (Number i) = show i
  show (Boolean b) = show b
  show (Ident i) = "%" ++ show i
  show (Abstr t) = "(\\. " ++ show t ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2
  show (AppPrim If [c, t, e]) =
    "if " ++ show c
      ++ " then " ++ show t
      ++ " else " ++ show e
  show (AppPrim Equal [a, b]) =
    show a ++ " = " ++ show b
  show (AppPrim Minus [a, b]) =
    "(" ++ show a ++ " - " ++ show b ++ ")"
  show (AppPrim Multi [a, b]) =
    "(" ++ show a ++ " * " ++ show b ++ ")"

shiftBy :: Int -> Int -> Term -> Term
shiftBy n c (Ident i) =
  if i < c
    then Ident i
    else Ident (i + n)
shiftBy n c (Abstr t) =
  Abstr (shiftBy n (c + 1) t)
shiftBy n c (App t1 t2) = 
  App (shiftBy n c t1) (shiftBy n c t2)
shiftBy n c (AppPrim p ts) =
  AppPrim p (map (shiftBy n c) ts)
shiftBy n c t = t

shiftUp :: Term -> Term
shiftUp = shiftBy 1 0

shiftDown :: Term -> Term
shiftDown = shiftBy (-1) 0

subtitude :: Int -> Term -> Term -> Term
subtitude a b (Ident x) =
  if a == x
    then b
    else Ident x
subtitude a b (Abstr t) =
  Abstr . subtitude (a + 1) (shiftUp b) $ t
subtitude a b (App t1 t2) =
  App (subtitude a b t1) (subtitude a b t2)
subtitude a b (AppPrim p ts) =
  AppPrim p (map (subtitude a b) ts)
subtitude a b t = t

callByName :: Term -> Maybe Term
callByName (App t1 t2) =
  case callByName t1 of
    Just t1' -> Just (App t1' t2)
    Nothing ->
      case t1 of
        Abstr _ -> Just (fromAbstr (shiftDown (subtitude (-1) t2 t1)))
        _ -> error "not function!"
callByName (AppPrim If [s, t, e]) =
  case callByName s of
    Nothing ->
      case s of
        Boolean True -> Just t
        Boolean False -> Just e
        _ -> error "not boolean!"
    Just s' -> Just (AppPrim If [s', t, e])
callByName (AppPrim prim xs) =
  case walk xs of
    Nothing -> Just (op xs)
    Just xs' -> Just (AppPrim prim xs')
  where walk (t:ts) =
          case callByName t of
            Nothing -> (t:) <$> walk ts
            Just t' -> Just (t':ts)
        walk [] = Nothing
        op [Number a, Number b] =
          case prim of
            Minus -> Number (a - b)
            Multi -> Number (a * b)
            Equal -> Boolean (a == b)
        op [a, b] =
          case prim of 
            Equal -> Boolean (a == b)
            _ -> error "not numbers!"
callByName _ = Nothing

bigStep :: (Term -> Maybe Term) -> Term -> Term
bigStep f t = loop t (f t)
  where loop a Nothing = a
        loop a (Just b) = loop b (f b)
        
lamY :: Term
lamY =
  Abstr (App (Abstr (App (Ident 1) (App (Ident 0) (Ident 0))))
             (Abstr (App (Ident 1) (App (Ident 0) (Ident 0)))))
-- Y = \f. (\x. f (x x)) (\x. f (x x))

lamFact :: Term
lamFact =
  Abstr (Abstr (AppPrim If [AppPrim Equal [Ident 0, Number 0],
                            Number 1,
                            AppPrim Multi [Ident 0,
                                           App (Ident 1)
                                               (AppPrim Minus [Ident 0, Number 1])]]))
-- fact = \self. \x. if x == 0 then 1 else x * self (x-1)

lamProgram :: Term
lamProgram = App (App lamY lamFact) (Number 4)

main :: IO ()
main = flip fix lamProgram $ \self prog -> do
  putStrLn . show $ prog
  case callByName prog of
    Nothing -> return ()
    Just prog' -> self prog'

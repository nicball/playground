module Main
import Control.Monad.State
import Data.Vect
import Data.Nat
import Data.SortedMap
import Data.List

data Wire : Type where
  MkWire : Int -> Wire

Eq Wire where
  (MkWire a) == (MkWire b) = a == b

Ord Wire where
  compare (MkWire a) (MkWire b) = compare a b

Show Wire where
  show (MkWire a) = "wire(" ++ show a ++ ")"

data Component : Type where
  Nand : (a : Wire) -> (b : Wire) -> (aNandB : Wire) -> Component
  One : (alwaysOne : Wire) -> Component
  Zero : (alwaysZero : Wire) -> Component
  Observer : (tag : String) -> Wire -> Component

Show Component where
  show (Nand a b c) = show c ++ "=" ++ "~(" ++ show a ++ "&" ++ show b ++ ")"
  show (One w) = show w ++ "=1"
  show (Zero w) = show w ++ "=0"
  show (Observer tag w) = tag ++ "=" ++ show w

Circuit : Type
Circuit = List Component

record BuilderState where
  constructor MkBuilderState
  wireCounter : Int
  circuit : List Component

CircuitBuilder : Type -> Type
CircuitBuilder = State BuilderState

runCircuitBuilder : CircuitBuilder a -> Circuit
runCircuitBuilder = circuit . execState (MkBuilderState 0 [])

newWire : CircuitBuilder Wire
newWire = do
  wire <- gets wireCounter
  modify record { wireCounter $= (+ 1) }
  pure (MkWire wire)

addComponents : List Component -> CircuitBuilder ()
addComponents ts = modify record { circuit $= (ts ++) }

observe : String -> Wire -> CircuitBuilder ()
observe tag w = addComponents [ Observer tag w ]

combinatorial : (Wire -> CircuitBuilder (List Component)) -> CircuitBuilder Wire
combinatorial f = do
  out <- newWire
  comps <- f out
  addComponents comps
  pure out

nand : Wire -> Wire -> CircuitBuilder Wire
nand a b = combinatorial \out =>
  pure [ Nand a b out ]

one : CircuitBuilder Wire
one = combinatorial \out =>
  pure [ One out ]

zero : CircuitBuilder Wire
zero = combinatorial \out =>
  pure [ Zero out ]

not : Wire -> CircuitBuilder Wire
not a = nand a !one

and : Wire -> Wire -> CircuitBuilder Wire
and a b = not !(nand a b)

or : Wire -> Wire -> CircuitBuilder Wire
or a b = nand !(not a) !(not b)

xor : Wire -> Wire -> CircuitBuilder Wire
xor a b = and !(nand a b) !(or a b)

nor : Wire -> Wire -> CircuitBuilder Wire
nor a b = not !(or a b)

halfAdder : Wire -> Wire -> CircuitBuilder (Wire, Wire)
halfAdder a b = [| (xor a b, and a b) |]

fullAdder : Wire -> Wire -> Wire -> CircuitBuilder (Wire, Wire)
fullAdder a b carryIn = do
  (sum1, carry1) <- halfAdder a b
  (sum2, carry2) <- halfAdder sum1 carryIn
  pure (sum2, !(or carry1 carry2))

Byte : Type
Byte = Vect 8 Wire

dFoldL : {n : Nat}
  -> (0 motive : Nat -> Type)
  -> (forall p. motive p -> a -> motive (S p))
  -> motive 0
  -> Vect n a
  -> motive n
dFoldL {n} motive f acc v = rewrite sym $ plusZeroRightNeutral n in helper acc v
  where
    helper : {q, m : Nat} -> motive m -> Vect q a -> motive (q + m)
    helper acc Nil = acc
    helper {q = S q, m} acc (x :: xs) = rewrite plusSuccRightSucc q m in helper (f acc x) xs

byteAdder : Byte -> Byte -> Wire -> CircuitBuilder (Byte, Wire)
byteAdder as bs carryIn = dFoldL (\n => CircuitBuilder (Vect n Wire, Wire)) (\ms, ab => f !ms ab) (pure (Nil, carryIn)) (zipWith (,) as bs)
  where
    f : (Vect n Wire, Wire) -> (Wire, Wire) -> CircuitBuilder (Vect (S n) Wire, Wire)
    f (os, c) (a, b) = do
      (o, c') <- fullAdder a b c
      pure (snoc os o, c')

boolToWire : Vect n Bool -> CircuitBuilder (Vect n Wire)
boolToWire = traverse \b =>
  case b of
    True => one
    False => zero

data Signal = High | Low | Undefined

Show Signal where
  show High = "1"
  show Low = "0"
  show Undefined = "x"

data CircuitState
  = Valid (SortedMap Wire Signal)
  | Invalid

Show CircuitState where
  show (Valid cstate) = show cstate
  show Invalid = "invalid circuit state"

Semigroup CircuitState where
  Invalid <+> _ = Invalid
  _ <+> Invalid = Invalid
  Valid a <+> Valid b = toCS . sequence $ mergeWith m (map Just a) (map Just b)
    where
      m : Maybe Signal -> Maybe Signal -> Maybe Signal
      m Nothing _ = Nothing
      m _ Nothing = Nothing
      m (Just Undefined) (Just x) = Just x
      m (Just x) (Just Undefined) = Just x
      m (Just High) (Just High) = Just High
      m (Just Low) (Just Low) = Just Low
      m (Just _) (Just _) = Nothing

      toCS : Maybe (SortedMap Wire Signal) -> CircuitState
      toCS (Just cs) = Valid cs
      toCS Nothing = Invalid

Monoid CircuitState where
  neutral = Valid empty

lookupSignal : Wire -> CircuitState -> Maybe Signal
lookupSignal w (Valid cs) = lookup w cs
lookupSignal w Invalid = Nothing

initialState : Circuit -> CircuitState
initialState = concat . map toMap 
  where
    toMap : Component -> CircuitState
    toMap (Nand a b c) = Valid . fromList $ [ (a, Undefined), (b, Undefined), (c, Undefined) ]
    toMap (One w) = Valid . fromList $ [ (w, High) ]
    toMap (Zero w) = Valid . fromList $ [ (w, Low) ]
    toMap (Observer _ w) = Valid . fromList $ [ (w, Undefined) ]

count : (n : Nat) -> Vect n Nat
count 0 = Nil
count (S n) = snoc (count n) n

exampleCircuit : Circuit
exampleCircuit = runCircuitBuilder $ do
  const2 <- boolToWire [ False, True, False, False, False, False, False, False ]
  const3 <- boolToWire [ True, True, False, False, False, False, False, False ]
  (shouldBe5, shouldBe0) <- byteAdder const2 const3 !zero
  let indexed5 = zipWith (,) shouldBe5 (count 8)
  traverse (\(w, n) => observe ("out[" ++ show n ++ "]") w) indexed5
  observe "out carry" shouldBe0

runCircuit : Circuit -> CircuitState-> CircuitState
runCircuit comps cstate = concat . map apply $ comps
  where
    signalNand : Signal -> Signal -> Signal
    signalNand Undefined _ = Undefined
    signalNand _ Undefined = Undefined
    signalNand High High = Low
    signalNand High Low = High
    signalNand Low High = High
    signalNand Low Low = High

    apply : Component -> CircuitState
    apply (Nand a b o) =
      let as = lookupSignal a cstate
          bs = lookupSignal b cstate
      in case (as, bs) of
        (Just asig, Just bsig) => Valid . fromList $
          [ (a, asig), (b, bsig), (o, signalNand asig bsig) ]
        _ => Invalid
    apply c = initialState [c]

Observations : Type
Observations = SortedMap String Signal

getObservations : Circuit -> CircuitState -> Observations
getObservations comps (Valid cstate) = fromList . concatMap ob $ comps
  where
    ob : Component -> List (String, Signal)
    ob (Observer tag w) =
      case lookup w cstate of
        Just sig => [ (tag, sig) ]
        Nothing => []
    ob _ = []
getObservations _ Invalid = empty

main : IO ()
main = map (const ()) . traverse (putStrLn . show) . map (getObservations exampleCircuit) . iterateN 50 (runCircuit exampleCircuit) . initialState $ exampleCircuit

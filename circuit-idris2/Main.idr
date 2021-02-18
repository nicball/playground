module Main

import Control.Monad.State
import Data.Fin
import Data.List
import Data.Nat
import Data.SortedMap
import Data.Vect

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
  Relay : (in' : Wire) -> (out : Wire) -> Component
  -- Boot : Wire -> Component

Show Component where
  show (Nand a b o) = show o ++ "=" ++ "~(" ++ show a ++ "&" ++ show b ++ ")"
  show (One w) = show w ++ "=1"
  show (Zero w) = show w ++ "=0"
  show (Observer tag w) = tag ++ "=" ++ show w
  show (Relay i o) = show o ++ "=" ++ show i
  -- show (Boot w) = show w ++ "=<boot>"

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

relay : Wire -> CircuitBuilder Wire
relay w = combinatorial \out =>
  pure [ Relay w out ]

relay_ : Wire -> Wire -> CircuitBuilder ()
relay_ i o = addComponents [ Relay i o ]

delay : Nat -> Wire -> CircuitBuilder Wire
delay 0 w = pure w
delay (S k) w = relay w >>= delay k

-- boot : Wire -> CircuitBuilder ()
-- boot w = addComponents [ Boot w ]

nand : Wire -> Wire -> CircuitBuilder Wire
nand a b = combinatorial \out =>
  pure [ Nand a b out ]

nand_ : Wire -> Wire -> Wire -> CircuitBuilder ()
nand_ a b o = addComponents [ Nand a b o ]

one : CircuitBuilder Wire
one = combinatorial \out =>
  pure [ One out ]

one_ : Wire -> CircuitBuilder ()
one_ w = addComponents [ One w ]

zero : CircuitBuilder Wire
zero = combinatorial \out =>
  pure [ Zero out ]

zero_ : Wire -> CircuitBuilder ()
zero_ w = addComponents [ Zero w ]

not : Wire -> CircuitBuilder Wire
not a = nand a !one

not_ : Wire -> Wire -> CircuitBuilder ()
not_ a na = addComponents [ Nand a !one na ]

and : Wire -> Wire -> CircuitBuilder Wire
and a b = not !(nand a b)

or : Wire -> Wire -> CircuitBuilder Wire
or a b = nand !(not a) !(not b)

xor : Wire -> Wire -> CircuitBuilder Wire
xor a b = and !(nand a b) !(or a b)

nor : Wire -> Wire -> CircuitBuilder Wire
nor a b = not !(or a b)

nor_ : Wire -> Wire -> Wire -> CircuitBuilder ()
nor_ a b o = not_ !(or a b) o

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

binsToConsts : Vect n (Fin 2) -> CircuitBuilder (Vect n Wire)
binsToConsts = traverse \b =>
  case b of
    1 => one
    0 => zero

-- binsToBootedWires : Vect n (Fin 2) -> CircuitBuilder (Vect n Wire)
-- binsToBootedWires = traverse \b => do
--   w <- newWire
--   if b == 1 then boot w else pure ()
--   pure w

clock : (halfWidthMinusOne : Nat) -> CircuitBuilder Wire
clock n = do
  w <- newWire
  i <- delay n w
  not_ i w
  pure i

gatedSRLatch_ : (set : Wire) -> (reset : Wire) -> (enable : Wire)
  -> (q : Wire) -> (nq : Wire) -> CircuitBuilder ()
gatedSRLatch_ s r e q nq= do
  s' <- and s e
  r' <- and r e
  nor_ r' nq q
  nor_ s' q nq
  pure ()

gatedSRLatch : (set : Wire) -> (reset : Wire) -> (enable : Wire) -> CircuitBuilder (Wire, Wire)
gatedSRLatch s r e = do
  q <- newWire
  nq <- newWire
  gatedSRLatch_ s r e q nq
  pure (q, nq)

gatedDLatch : (data' : Wire) -> (enable : Wire) -> CircuitBuilder (Wire, Wire)
gatedDLatch d e = gatedSRLatch d !(not d) e

dFlipFlop_ : (data' : Wire) -> (clock : Wire) -> (dataNext : Wire) -> CircuitBuilder ()
dFlipFlop_ d clk dn = do
  (q, nq) <- gatedDLatch d !(not clk)
  dummy <- newWire
  gatedSRLatch_ q nq clk dn dummy

dFlipFlop : (data' : Wire) -> (clock : Wire) -> CircuitBuilder Wire
dFlipFlop d clk = do
  dn <- newWire
  dFlipFlop_ d clk dn
  pure dn

data Signal = High | Low

Show Signal where
  show High = "1"
  show Low = "0"

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
      m (Just High) (Just High) = Just High
      m (Just Low) (Just Low) = Just Low
      m _ _ = Nothing

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
    toMap (Nand a b c) = Valid . fromList $ [ (a, Low), (b, Low), (c, Low) ]
    toMap (One w) = Valid . fromList $ [ (w, Low) ]
    toMap (Zero w) = Valid . fromList $ [ (w, Low) ]
    toMap (Observer _ w) = Valid . fromList $ [ (w, Low) ]
    toMap (Relay i o) = Valid . fromList $ [ (i, Low), (o, Low) ]
    -- toMap (Boot w) = Valid . fromList $ [ (w, High) ]

count : (n : Nat) -> Vect n Nat
count 0 = Nil
count (S n) = snoc (count n) n

runCircuit : Circuit -> CircuitState-> CircuitState
runCircuit comps cstate = concat . map drive $ comps
  where
    signalNand : Signal -> Signal -> Signal
    signalNand High High = Low
    signalNand High Low = High
    signalNand Low High = High
    signalNand Low Low = High

    drive : Component -> CircuitState
    drive (Nand a b o) =
      let as = lookupSignal a cstate
          bs = lookupSignal b cstate
      in case (as, bs) of
        (Just asig, Just bsig) => Valid . fromList $ [ (o, signalNand asig bsig) ]
        _ => Invalid
    drive (One w) = Valid . fromList $ [ (w, High) ]
    drive (Zero w) = Valid . fromList $ [ (w, Low) ]
    drive (Observer _ w) = Valid empty
    drive (Relay i o) =
      case lookupSignal i cstate of
        Just isig => Valid . fromList $ [ (o, isig) ]
        Nothing => Invalid
    -- drive (Boot w) = Valid empty

Observations : Type
Observations = List (String, Signal)

getObservations : Circuit -> CircuitState -> Observations
getObservations comps (Valid cstate) = concatMap ob $ comps
  where
    ob : Component -> List (String, Signal)
    ob (Observer tag w) =
      case lookup w cstate of
        Just sig => [ (tag, sig) ]
        Nothing => []
    ob _ = []
getObservations _ Invalid = []

putObservationsLn : Observations -> IO ()
putObservationsLn = (>> putChar '\n') . traverse f 
  where
    f : (String, Signal) -> IO ()
    f (tag, sig) = putStr $ tag ++ "=" ++ show sig ++ ", "

exampleAdder : Circuit
exampleAdder = runCircuitBuilder $ do
  const2 <- binsToConsts [ 0, 1, 0, 0, 0, 0, 0, 0 ]
  const3 <- binsToConsts [ 1, 1, 0, 0, 0, 0, 0, 0 ]
  (shouldBe5, shouldBe0) <- byteAdder const2 const3 !zero
  let indexed5 = zipWith (,) shouldBe5 (count 8)
  traverse (\(w, n) => observe ("out[" ++ show n ++ "]") w) indexed5
  observe "out_carry" shouldBe0

exampleClock : Nat -> Circuit
exampleClock n = runCircuitBuilder $ do
  c <- clock n
  observe "clock" c

exampleLatch : Circuit
exampleLatch = runCircuitBuilder $ do
  (q, nq) <- gatedSRLatch !zero !one !one
  observe "q" q
  observe "~q" nq

exampleDff : Circuit
exampleDff = runCircuitBuilder $ do
  clk <- clock 40
  observe "clk" clk
  dFlipFlop !one clk >>= observe "dn"

main : IO ()
main =
  let exm = exampleDff
  in map (const ()) . traverse (putObservationsLn . getObservations exm) . iterateN 200 (runCircuit exm) . initialState $ exm

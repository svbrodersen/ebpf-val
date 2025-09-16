{-# LANGUAGE FlexibleInstances #-}

module Definitions where

import Data.Array as Array
import Data.Map as Map
import Data.Set (Set)
import Ebpf.Asm
import Ebpf_cfg (Label)

data Bound = NegInf | Val Integer | PosInf deriving (Eq, Ord)

addBound :: Bound -> Bound -> BottomM Bound
addBound NegInf (Val _) = Value NegInf
addBound (Val _) NegInf = Value NegInf
addBound PosInf (Val _) = Value PosInf
addBound (Val _) PosInf = Value PosInf
addBound (Val x) (Val y) = return $ Val (x + y)
-- The rest are undefined
addBound _ _ = Bottom

subBound :: Bound -> Bound -> BottomM Bound
subBound NegInf (Val _) = Value NegInf
subBound (Val _) PosInf = Value NegInf
subBound PosInf (Val _) = Value PosInf
subBound (Val _) NegInf = Value PosInf
subBound (Val x) (Val y) = return $ Val (x - y)
-- Rest undefined
subBound _ _ = Bottom

mulBound :: Bound -> Bound -> BottomM Bound
mulBound NegInf NegInf = Value PosInf
mulBound NegInf PosInf = Value NegInf
mulBound PosInf NegInf = Value NegInf
mulBound PosInf PosInf = Value PosInf
mulBound NegInf (Val x)
  | x > 0 = Value NegInf
  | x < 0 = Value PosInf
  | x == 0 = Value $ Val 0
mulBound PosInf (Val x)
  | x > 0 = Value PosInf
  | x < 0 = Value NegInf
  -- Non-standard mentioned in Mine
  | x == 0 = Value $ Val 0
mulBound (Val x) NegInf = mulBound NegInf (Val x)
mulBound (Val x) PosInf = mulBound PosInf (Val x)
mulBound (Val x) (Val y) = Value $ Val (x * y)
mulBound _ _ = Bottom

divBound :: Bound -> Bound -> BottomM Bound
divBound (Val x) (Val y) = Value $ Val (x `div` y)
divBound _ PosInf = Value $ Val 0
divBound _ NegInf = Value $ Val 0
divBound NegInf (Val y)
  | y > 0 = Value NegInf
  | y < 0 = Value PosInf
  | y == 0 = Bottom
divBound PosInf (Val y)
  | y > 0 = Value PosInf
  | y < 0 = Value NegInf
  | y == 0 = Bottom
divBound _ _ = Bottom

negateBound :: Bound -> BottomM Bound
negateBound NegInf = Value PosInf
negateBound PosInf = Value NegInf
negateBound (Val x) = Value $ Val (-x)

instance Show Bound where
  show NegInf = "-inf"
  show (Val i) = show i
  show PosInf = "+inf"

data Interval = Interval Bound Bound deriving (Eq, Ord)

isSubsetEqual :: Interval -> Interval -> Bool
isSubsetEqual (Interval a b) (Interval c d) = (a >= c) && (b <= d)

unionInterval :: Interval -> Interval -> IntervalM
unionInterval (Interval a b) (Interval c d) =
  let lo = min a c
      hi = max b d
   in Value $ Interval lo hi

intersectInterval :: Interval -> Interval -> IntervalM
intersectInterval (Interval a b) (Interval c d) =
  let lo = max a c
      hi = min b d
   in if lo <= hi
        then Value $ Interval lo hi
        else Bottom

-- Abstract operators
addInterval :: Interval -> Interval -> IntervalM
addInterval (Interval l1 u1) (Interval l2 u2) = do
  l <- addBound l1 l2
  u <- addBound u1 u2
  Value $ Interval l u

subInterval :: Interval -> Interval -> IntervalM
subInterval (Interval l1 u1) (Interval l2 u2) = do
  l <- subBound l1 u2
  u <- subBound u1 l2
  Value $ Interval l u

mulInterval :: Interval -> Interval -> IntervalM
mulInterval (Interval l1 u1) (Interval l2 u2) = do
  p1 <- mulBound l1 l2
  p2 <- mulBound l1 u2
  p3 <- mulBound u1 l2
  p4 <- mulBound u1 u2
  Value $ Interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])

divInterval :: Interval -> Interval -> IntervalM
divInterval (Interval a b) (Interval c d)
  | c >= Val 1 = do
      l <- divBound a c
      u <- divBound b d
      Value $ Interval (min l u) (max l u)
  | d <= Val (-1) = do
      l <- divBound b c
      u <- divBound a d
      Value $ Interval (min l u) (max l u)
  | otherwise = do
      let ab = Interval a b
          cd = Interval c d
      t1 <- intersectInterval cd (Interval (Val 1) PosInf)
      t2 <- intersectInterval cd (Interval NegInf (Val (-1)))
      first <- divInterval ab t1
      second <- divInterval ab t2
      unionInterval first second

negateInterval :: Interval -> IntervalM
negateInterval (Interval l u) = do
  l' <- negateBound u
  u' <- negateBound l
  Value $ Interval l' u'

wideningInterval :: Interval -> Interval -> IntervalM
wideningInterval (Interval l1 u1) (Interval l2 u2) =
  let l = if l2 < l1 then NegInf else l1
      u = if u2 > u1 then PosInf else u1
   in Value $ Interval l u

bitWiseInterval :: Interval -> Interval -> IntervalM
bitWiseInterval _ _ = Value $ Interval NegInf PosInf

-- Comparison operators
equalInterval :: Interval -> Interval -> (IntervalM, IntervalM)
equalInterval i1 i2 =
  let first = intersectInterval i1 i2
      second = first
   in (first, second)

lessThanInterval :: Interval -> Interval -> (IntervalM, IntervalM)
lessThanInterval (Interval a b) (Interval c d) =
  let i1 = intersectInterval (Interval a b) (Interval NegInf (subVal d 1))
      i2 = intersectInterval (Interval c d) (Interval (addVal a 1) PosInf)
   in (i1, i2)
 where
  subVal (Val x) y = Val (x - y)
  subVal PosInf _ = PosInf
  subVal NegInf _ = NegInf
  addVal (Val x) y = Val (x + y)
  addVal PosInf _ = PosInf
  addVal NegInf _ = NegInf

lessThanEqualInterval :: Interval -> Interval -> (IntervalM, IntervalM)
lessThanEqualInterval (Interval a b) (Interval c d) =
  let first = intersectInterval (Interval a b) (Interval NegInf d)
      second = intersectInterval (Interval a PosInf) (Interval c d)
   in (first, second)

notEqualInterval :: Interval -> Interval -> (IntervalM, IntervalM)
notEqualInterval i1@(Interval a b) i2@(Interval c d) =
  let overlap = max a c <= min b d
   in if overlap
        then (subInterval i1 i2, subInterval i2 i1)
        else (Value i1, Value i2)

instance Show Interval where
  show (Interval lo hi) = "[" ++ show lo ++ ", " ++ show hi ++ "]"

-- Create BottomM monad. Just the same as Maybe monad with Bottom and Value instead of Nothing Just.
data BottomM a = Bottom | Value a
  deriving (Eq, Ord)

instance (Show a) => Show (BottomM a) where
  show Bottom = "⊥"
  show (Value x) = show x

instance Functor BottomM where
  fmap _ Bottom = Bottom
  fmap f (Value x) = Value (f x)

instance Applicative BottomM where
  pure = Value
  Bottom <*> _ = Bottom
  _ <*> Bottom = Bottom
  Value f <*> Value x = Value (f x)

instance Monad BottomM where
  Bottom >>= _ = Bottom
  Value x >>= f = f x

instance Fail.MonadFail BottomM where
  fail _ = Bottom

type IntervalM = BottomM Interval

wrapFunc :: (Interval -> Interval -> IntervalM) -> IntervalM -> IntervalM -> IntervalM
wrapFunc f m1 m2 = do
  i1 <- m1
  i2 <- m2
  f i1 i2

-- Order structure
isSubsetEqualM :: IntervalM -> IntervalM -> Bool
isSubsetEqualM Bottom _ = True
isSubsetEqualM _ Bottom = False
isSubsetEqualM (Value i1) (Value i2) = isSubsetEqual i1 i2

unionIntervalM :: IntervalM -> IntervalM -> IntervalM
unionIntervalM = wrapFunc unionInterval

intersectIntervalM :: IntervalM -> IntervalM -> IntervalM
intersectIntervalM = wrapFunc intersectInterval

-- Abstract operators
addIntervalM :: IntervalM -> IntervalM -> IntervalM
addIntervalM = wrapFunc addInterval

subIntervalM :: IntervalM -> IntervalM -> IntervalM
subIntervalM = wrapFunc subInterval

mulIntervalM :: IntervalM -> IntervalM -> IntervalM
mulIntervalM = wrapFunc mulInterval

divIntervalM :: IntervalM -> IntervalM -> IntervalM
divIntervalM = wrapFunc divInterval

negateIntervalM :: IntervalM -> IntervalM
negateIntervalM m1 = do
  i <- m1
  negateInterval i

wideningIntervalM :: IntervalM -> IntervalM -> IntervalM
wideningIntervalM i Bottom = i
wideningIntervalM Bottom i = i
wideningIntervalM m1 m2 = wrapFunc wideningInterval m1 m2

bitWiseIntervalM :: IntervalM -> IntervalM -> IntervalM
bitWiseIntervalM = wrapFunc bitWiseInterval

instance Num IntervalM where
  (+) = addIntervalM
  (-) = subIntervalM
  (*) = mulIntervalM
  negate = negateIntervalM
  abs _ = Bottom
  signum _ = Bottom
  fromInteger i = Value $ Interval (Val i) (Val i)

type Mem = Array Int IntervalM

data State = State
  { registers :: Array Int IntervalM
  , memory :: Mem
  , dependencies :: Map.Map Label (Set Label)
  }
  deriving (Eq)

instance Show State where
  show s =
    let (lo, hi) = bounds $ memory s
     in "Regs: "
          ++ show (Array.elems $ registers s)
          ++ "\n"
          ++ "Mem: "
          ++ show (zip [lo .. hi] (Array.elems (memory s)))

numRegs :: Int
numRegs = 12

memSize :: Int
memSize = 512

topIntervalM :: IntervalM
topIntervalM = Value $ Interval NegInf PosInf

-- Create the initial state with 512 memory cells all set to top
initState :: State
initState =
  State
    { registers =
        array
          (0, numRegs - 1)
          [(i, topIntervalM) | i <- [0 .. numRegs - 1]]
    , memory =
        array
          (0, memSize - 1)
          [ (i, topIntervalM)
          | i <- [0 .. memSize - 1]
          ]
    , dependencies = empty
    }

{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Map as Map
import Ebpf.Asm
import GHC.Arr

data Bound = NegInf | Val Int | PosInf deriving (Eq, Ord)

addBound :: Bound -> Bound -> BottomM Bound
addBound NegInf _ = Value NegInf
addBound _ NegInf = Value NegInf
addBound PosInf _ = Value PosInf
addBound _ PosInf = Value PosInf
addBound (Val x) (Val y) = return $ Val (x + y)

subBound :: Bound -> Bound -> BottomM Bound
subBound NegInf _ = Value NegInf
subBound _ PosInf = Value NegInf
subBound PosInf _ = Value PosInf
subBound _ NegInf = Value PosInf
subBound (Val x) (Val y) = return $ Val (x - y)

mulBound :: Bound -> Bound -> BottomM Bound
mulBound NegInf NegInf = Value PosInf
mulBound NegInf PosInf = Value NegInf
mulBound PosInf NegInf = Value NegInf
mulBound PosInf PosInf = Value PosInf
mulBound NegInf (Val x)
  | x > 0 = Value NegInf
  | x < 0 = Value PosInf
  | x == 0 = Bottom
mulBound PosInf (Val x)
  | x > 0 = Value PosInf
  | x < 0 = Value NegInf
  | x == 0 = Bottom
mulBound (Val x) NegInf = mulBound NegInf (Val x)
mulBound (Val x) PosInf = mulBound PosInf (Val x)
mulBound (Val x) (Val y) = return $ Val (x * y)
mulBound _ _ = Bottom

divBound :: Bound -> Bound -> BottomM Bound
divBound _ (Val 0) = Bottom
divBound (Val x) (Val y) = return $ Val (x `div` y)
divBound NegInf NegInf = Bottom
divBound PosInf PosInf = Bottom
divBound NegInf PosInf = return $ Val 0
divBound PosInf NegInf = return $ Val 0
divBound NegInf (Val y)
  | y > 0 = Value NegInf
  | y < 0 = Value PosInf
divBound PosInf (Val y)
  | y > 0 = Value PosInf
  | y < 0 = Value NegInf
divBound (Val _) NegInf = return $ Val 0
divBound (Val _) PosInf = return $ Val 0
divBound _ _ = Bottom

negateBound :: Bound -> BottomM Bound
negateBound NegInf = Value PosInf
negateBound PosInf = Value NegInf
negateBound (Val x) = return $ Val (-x)

instance Show Bound where
  show NegInf = "-inf"
  show (Val i) = show i
  show PosInf = "+inf"

data Interval = Interval Bound Bound deriving (Eq, Ord)

-- Order structure
isSubsetEqual :: Interval -> Interval -> Bool
isSubsetEqual (Interval a b) (Interval c d) = a >= c && b <= d

unionInterval :: Interval -> Interval -> IntervalM
unionInterval (Interval a b) (Interval c d) =
  let lo = max a c
      hi = min b d
   in if lo < hi
        then return $ Interval lo hi
        else Bottom

intersectInterval :: Interval -> Interval -> IntervalM
intersectInterval (Interval a b) (Interval c d) =
  let lo = max a c
      hi = min b d
   in if lo < hi
        then return $ Interval lo hi
        else Bottom

-- Abstract operators
addInterval :: Interval -> Interval -> IntervalM
addInterval (Interval l1 u1) (Interval l2 u2) = do
  l <- addBound l1 l2
  u <- addBound u1 u2
  return $ Interval l u

subInterval :: Interval -> Interval -> IntervalM
subInterval (Interval l1 u1) (Interval l2 u2) = do
  l <- subBound l1 u2
  u <- subBound u1 l2
  return $ Interval l u

mulInterval :: Interval -> Interval -> IntervalM
mulInterval (Interval l1 u1) (Interval l2 u2) = do
  p1 <- mulBound l1 l2
  p2 <- mulBound l1 u2
  p3 <- mulBound u1 l2
  p4 <- mulBound u1 u2
  return $ Interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4])

divInterval :: Interval -> Interval -> IntervalM
divInterval (Interval a b) (Interval c d)
  | c >= Val 1 = do
      l <- divBound a c
      u <- divBound b d
      return $ Interval (min l u) (max l u)
  | d <= Val (-1) = do
      l <- divBound b c
      u <- divBound a d
      return $ Interval (min l u) (max l u)
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
  return $ Interval l' u'

wideningInterval :: Interval -> Interval -> IntervalM
wideningInterval (Interval l1 u1) (Interval l2 u2) =
  let l
        | l1 < l2 = NegInf
        | l1 > l2 = PosInf
        | otherwise = l1
      u
        | u1 < u2 = NegInf
        | u1 > u2 = PosInf
        | otherwise = u1
   in return $ Interval l u

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

type IntervalM = BottomM Interval

-- Order structure
isSubsetEqualM :: IntervalM -> IntervalM -> Bool
isSubsetEqualM Bottom _ = True
isSubsetEqualM _ Bottom = False
isSubsetEqualM (Value i1) (Value i2) = isSubsetEqual i1 i2

unionIntervalM :: IntervalM -> IntervalM -> IntervalM
unionIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  unionInterval i1 i2

intersectIntervalM :: IntervalM -> IntervalM -> IntervalM
intersectIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  intersectInterval i1 i2

-- Abstract operators
addIntervalM :: IntervalM -> IntervalM -> IntervalM
addIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  addInterval i1 i2

subIntervalM :: IntervalM -> IntervalM -> IntervalM
subIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  subInterval i1 i2

mulIntervalM :: IntervalM -> IntervalM -> IntervalM
mulIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  mulInterval i1 i2

divIntervalM :: IntervalM -> IntervalM -> IntervalM
divIntervalM m1 m2 = do
  i1 <- m1
  i2 <- m2
  divInterval i1 i2

negateIntervalM :: IntervalM -> IntervalM
negateIntervalM m1 = do
  i <- m1
  negateInterval i

instance Num IntervalM where
  (+) = addIntervalM
  (-) = subIntervalM
  (*) = mulIntervalM
  negate = negateIntervalM
  abs _ = Bottom
  signum _ = Bottom
  fromInteger _ = Bottom

type Mem = Array Int IntervalM

data State = State
  { registers :: Map.Map Reg IntervalM
  , memory :: Mem
  }
  deriving (Eq)

instance Show State where
  show s =
    let (lo, hi) = bounds $ memory s
     in "Regs: "
          ++ show (Map.toList $ registers s)
          ++ "\n"
          ++ "Mem: "
          ++ show (zip [lo .. hi] (GHC.Arr.elems (memory s)))

numRegs :: Int
numRegs = 11

memSize :: Int
memSize = 512

topInterval :: Interval
topInterval = Interval NegInf PosInf

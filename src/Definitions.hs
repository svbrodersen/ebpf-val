{-# LANGUAGE FlexibleInstances #-}

module Definitions where

import Data.Array as Array

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
subBound (Val x) (Val y) = return $ Val (x - y)
subBound NegInf (Val _) = Value NegInf
subBound (Val _) PosInf = Value NegInf
subBound PosInf (Val _) = Value PosInf
subBound (Val _) NegInf = Value PosInf
subBound NegInf PosInf = Value NegInf
subBound PosInf NegInf = Value PosInf
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
divBound (Val x) (Val y)
  | y == 0 = Bottom
  | otherwise = Value $ Val (x `div` y)
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
divInterval ab@(Interval a b) cd@(Interval c d)
  | Val 1 <= c = do
      ac <- divBound a c
      ad <- divBound a d
      bc <- divBound b c
      bd <- divBound b d
      Value $ Interval (min ac ad) (max bc bd)
  | d <= Val (-1) = do
      bc <- divBound b c
      bd <- divBound b d
      ac <- divBound a c
      ad <- divBound a d
      Value $ Interval (min bc bd) (max ac ad)
  | otherwise =
      let first = do
            t1 <- intersectInterval cd (Interval (Val 1) PosInf)
            divInterval ab t1
          second = do
            t2 <- intersectInterval cd (Interval NegInf (Val (-1)))
            divInterval ab t2
       in unionIntervalM first second

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
equalInterval :: Interval -> Interval -> BottomM (Interval, Interval)
equalInterval i1 i2 = do
  first <- intersectInterval i1 i2
  return (first, first)

lessThanInterval :: Interval -> Interval -> BottomM (Interval, Interval)
lessThanInterval i1@(Interval a _) i2@(Interval _ d) = do
  i1' <- intersectInterval i1 (Interval NegInf (subVal d 1))
  i2' <- intersectInterval (Interval (addVal a 1) PosInf) i2
  return (i1', i2')
 where
  subVal (Val x) y = Val (x - y)
  subVal x _ = x
  addVal (Val x) y = Val (x + y)
  addVal x _ = x

lessThanEqualInterval :: Interval -> Interval -> BottomM (Interval, Interval)
lessThanEqualInterval i1@(Interval a _) i2@(Interval _ d) = do
  i1' <- intersectInterval i1 (Interval NegInf d)
  i2' <- intersectInterval (Interval a PosInf) i2
  return (i1', i2')

greaterThanInterval :: Interval -> Interval -> BottomM (Interval, Interval)
greaterThanInterval i1 i2 = do
  i1' <- negateInterval i1
  i2' <- negateInterval i2
  (i1'', i2'') <- lessThanInterval i1' i2'
  r1 <- negateInterval i1''
  r2 <- negateInterval i2''
  return (r1, r2)

greaterThanEqualInterval :: Interval -> Interval -> BottomM (Interval, Interval)
greaterThanEqualInterval i1 i2 = do
  i1' <- negateInterval i1
  i2' <- negateInterval i2
  (i1'', i2'') <- lessThanEqualInterval i1' i2'
  r1 <- negateInterval i1''
  r2 <- negateInterval i2''
  return (r1, r2)

notEqualInterval :: Interval -> Interval -> BottomM (Interval, Interval)
notEqualInterval i1@(Interval a b) i2@(Interval c d)
  -- They are exactly the same, so both bottom
  | a == c || b == d = Bottom
  | otherwise = do
      let intersect = intersectInterval i1 i2
       in case intersect of
            -- No intersection so they stay the same
            Bottom -> Value (i1, i2)
            -- Intersection, so we remove the intersection elements from each.
            Value int -> do
              t1 <- subInterval i1 int
              t2 <- subInterval i2 int
              return (t1, t2)

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

wrapFunc :: (Interval -> Interval -> BottomM a) -> IntervalM -> IntervalM -> BottomM a
wrapFunc f m1 m2 = do
  i1 <- m1
  i2 <- m2
  f i1 i2

isSubsetEqualM :: IntervalM -> IntervalM -> Bool
isSubsetEqualM Bottom _ = True
isSubsetEqualM _ Bottom = False
isSubsetEqualM (Value i1) (Value i2) = isSubsetEqual i1 i2

-- Union is the one case, where if we have bottom, then we can still do more computation on it.
unionIntervalM :: IntervalM -> IntervalM -> IntervalM
unionIntervalM m1 m2 =
  case (m1, m2) of
    (Bottom, Value i2) -> Value i2
    (Value i1, Bottom) -> Value i1
    (Value i1, Value i2) -> unionInterval i1 i2
    (_, _) -> Bottom

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

lessThanEqualIntervalM :: IntervalM -> IntervalM -> BottomM (Interval, Interval)
lessThanEqualIntervalM = wrapFunc lessThanEqualInterval

lessThanIntervalM :: IntervalM -> IntervalM -> BottomM (Interval, Interval)
lessThanIntervalM = wrapFunc lessThanInterval

greaterThanIntervalM :: IntervalM -> IntervalM -> BottomM (Interval, Interval)
greaterThanIntervalM = wrapFunc greaterThanInterval

greaterThanEqualIntervalM :: IntervalM -> IntervalM -> BottomM (Interval, Interval)
greaterThanEqualIntervalM = wrapFunc greaterThanEqualInterval

wideningIntervalM :: IntervalM -> IntervalM -> IntervalM
wideningIntervalM i Bottom = i
wideningIntervalM Bottom i = i
wideningIntervalM m1 m2 = wrapFunc wideningInterval m1 m2

narrowingInterval :: Interval -> Interval -> IntervalM
narrowingInterval (Interval l1 u1) (Interval l2 u2) =
  let l = if l1 == NegInf then l2 else l1
      u = if u1 == PosInf then u2 else u1
   in Value $ Interval l u

narrowingIntervalM :: IntervalM -> IntervalM -> IntervalM
narrowingIntervalM i Bottom = i
narrowingIntervalM Bottom i = i
narrowingIntervalM m1 m2 = wrapFunc narrowingInterval m1 m2

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
  }
  deriving (Eq)

instance Show State where
  show s =
    let (lo, hi) = bounds $ memory s
     in "Regs: "
          ++ show (Array.elems $ registers s)
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
    }

unionArray :: Array Int IntervalM -> Array Int IntervalM -> Array Int IntervalM
unionArray a1 a2 =
  let (lo, hi) = bounds a1
   in array (lo, hi) [(i, unionIntervalM (a1 Array.! i) (a2 Array.! i)) | i <- [lo .. hi]]

unionState :: State -> State -> State
unionState s1 s2 =
  State
    { registers = unionArray (registers s1) (registers s2)
    , memory = unionArray (memory s1) (memory s2)
    }

bottomState :: State
bottomState =
  State
    { registers = listArray (0, numRegs - 1) (repeat Bottom)
    , memory = listArray (0, memSize - 1) (repeat Bottom)
    }

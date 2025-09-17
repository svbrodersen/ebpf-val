module BoundSpec where

import Data.Array as Array
import Definitions
import Ebpf.Asm
import Ebpf_cfg
import Interpreter
import Test.Hspec

spec :: Spec
spec = do
  describe "Definitions" $ do
    describe "Bound Operations" $ do
      it "adds bounds correctly" $ do
        addBound (Val 5) (Val 10) `shouldBe` Value (Val 15)
        addBound NegInf (Val 10) `shouldBe` Value NegInf
        addBound PosInf (Val 10) `shouldBe` Value PosInf
        addBound (Val 10) NegInf `shouldBe` Value NegInf
        addBound (Val 10) PosInf `shouldBe` Value PosInf
      it "subtracts bounds correctly" $ do
        subBound (Val 10) (Val 5) `shouldBe` Value (Val 5)
        subBound NegInf (Val 10) `shouldBe` Value NegInf
        subBound PosInf (Val 10) `shouldBe` Value PosInf
        subBound (Val 10) NegInf `shouldBe` Value PosInf
        subBound (Val 10) PosInf `shouldBe` Value NegInf
      it "multiplies bounds correctly" $ do
        mulBound (Val 5) (Val 10) `shouldBe` Value (Val 50)
        mulBound NegInf (Val 10) `shouldBe` Value NegInf
        mulBound PosInf (Val 10) `shouldBe` Value PosInf
        mulBound (Val 10) NegInf `shouldBe` Value NegInf
        mulBound (Val 10) PosInf `shouldBe` Value PosInf
        mulBound NegInf (Val (-10)) `shouldBe` Value PosInf
        mulBound PosInf (Val (-10)) `shouldBe` Value NegInf
      it "divides bounds correctly" $ do
        divBound (Val 10) (Val 5) `shouldBe` Value (Val 2)
        divBound NegInf (Val 2) `shouldBe` Value NegInf
        divBound PosInf (Val 2) `shouldBe` Value PosInf
        divBound NegInf (Val (-2)) `shouldBe` Value PosInf
        divBound PosInf (Val (-2)) `shouldBe` Value NegInf

    describe "Interval Operations" $ do
      let i1 = Interval (Val 2) (Val 10)
      let i2 = Interval (Val 5) (Val 15)
      let i3 = Interval (Val (-5)) (Val 5)
      let i4 = Interval (Val 0) (Val 0)
      let i5 = Interval (Val (-10)) (Val (-2))
      let i_neg_inf = Interval NegInf (Val 5)
      let i_pos_inf = Interval (Val 5) PosInf
      let i_inf = Interval NegInf PosInf

      it "unions intervals correctly" $ do
        unionInterval i1 i2 `shouldBe` Value (Interval (Val 2) (Val 15))
        unionInterval i1 i3 `shouldBe` Value (Interval (Val (-5)) (Val 10))
        unionInterval i2 i3 `shouldBe` Value (Interval (Val (-5)) (Val 15))
        -- subset
        unionInterval i1 (Interval (Val 3) (Val 7)) `shouldBe` Value (Interval (Val 2) (Val 10))
        -- disjoint
        unionInterval i1 (Interval (Val 11) (Val 20)) `shouldBe` Value (Interval (Val 2) (Val 20))
        -- adjacent
        unionInterval i1 (Interval (Val 11) (Val 20)) `shouldBe` Value (Interval (Val 2) (Val 20))

      it "intersects intervals correctly" $ do
        intersectInterval i1 i2 `shouldBe` Value (Interval (Val 5) (Val 10))
        intersectInterval i1 i3 `shouldBe` Value (Interval (Val 2) (Val 5))
        intersectInterval i2 i3 `shouldBe` Value (Interval (Val 5) (Val 5))
        -- disjoint
        intersectInterval i1 i5 `shouldBe` Bottom
        -- adjacent
        intersectInterval i1 (Interval (Val 11) (Val 12)) `shouldBe` Bottom
        -- subset
        intersectInterval i1 (Interval (Val 4) (Val 6)) `shouldBe` Value (Interval (Val 4) (Val 6))

      it "adds intervals correctly" $ do
        addInterval i1 i2 `shouldBe` Value (Interval (Val 7) (Val 25))
        addInterval i1 i3 `shouldBe` Value (Interval (Val (-3)) (Val 15))
        addInterval i3 i5 `shouldBe` Value (Interval (Val (-15)) (Val 3))
        addInterval i_neg_inf i1 `shouldBe` Value (Interval NegInf (Val 15))
        addInterval i_pos_inf i1 `shouldBe` Value (Interval (Val 7) PosInf)
        addInterval i_neg_inf i_pos_inf `shouldBe` Value (Interval NegInf PosInf)

      it "subtracts intervals correctly" $ do
        subInterval i1 i2 `shouldBe` Value (Interval (Val (-13)) (Val 5))
        subInterval i1 i3 `shouldBe` Value (Interval (Val (-3)) (Val 15))
        subInterval i3 i5 `shouldBe` Value (Interval (Val (-3)) (Val 15))
        subInterval i_neg_inf i1 `shouldBe` Value (Interval NegInf (Val 3))
        subInterval i_pos_inf i1 `shouldBe` Value (Interval (Val (-5)) PosInf)
        -- This is undefined
        subInterval i_neg_inf i_pos_inf `shouldBe` Bottom

      it "multiplies intervals correctly" $ do
        mulInterval i1 i2 `shouldBe` Value (Interval (Val 10) (Val 150))
        mulInterval i1 i3 `shouldBe` Value (Interval (Val (-50)) (Val 50))
        mulInterval i3 i5 `shouldBe` Value (Interval (Val (-50)) (Val 50))
        mulInterval i3 i4 `shouldBe` Value (Interval (Val 0) (Val 0))
        mulInterval i_neg_inf i3 `shouldBe` Value (Interval NegInf PosInf)
        mulInterval i_pos_inf i3 `shouldBe` Value (Interval NegInf PosInf)
        mulInterval i_neg_inf (Interval (Val 0) (Val 0)) `shouldBe` Value (Interval (Val 0) (Val 0))
        mulInterval i_pos_inf (Interval (Val 0) (Val 0)) `shouldBe` Value (Interval (Val 0) (Val 0))
        mulInterval i_neg_inf (Interval (Val 2) (Val 5)) `shouldBe` Value (Interval NegInf (Val 25))
        mulInterval i_pos_inf (Interval (Val 2) (Val 5)) `shouldBe` Value (Interval (Val 10) PosInf)
        mulInterval i_neg_inf (Interval (Val (-5)) (Val (-2))) `shouldBe` Value (Interval (Val (-25)) PosInf)
        mulInterval i_pos_inf (Interval (Val (-5)) (Val (-2))) `shouldBe` Value (Interval NegInf (Val (-10)))

      it "divides intervals correctly" $ do
        divInterval i1 i2 `shouldBe` Value (Interval (Val 0) (Val 2))
        divInterval i2 i1 `shouldBe` Value (Interval (Val 0) (Val 7))
        divInterval i3 i1 `shouldBe` Value (Interval (Val (-3)) (Val 2))
        -- Divisor is [1, inf]
        divInterval (Interval (Val 10) (Val 20)) (Interval (Val 1) PosInf) `shouldBe` Value (Interval (Val 0) (Val 20))
        -- Divisor is [-inf, -1]
        divInterval (Interval (Val 10) (Val 20)) (Interval NegInf (Val (-1))) `shouldBe` Value (Interval (Val (-20)) (Val 0))
        -- Divisor contains 0 and spans positive and negative
        divInterval i1 i3 `shouldBe` Value (Interval (Val (-10)) (Val 10))
        divInterval (Interval (Val 10) (Val 20)) (Interval (Val (-2)) (Val 2)) `shouldBe` Value (Interval (Val (-20)) (Val 20))

      it "divides intervals when divisor contains 0 - Bottom" $ do
        divInterval (Interval (Val 10) (Val 20)) (Interval (Val 0) (Val 5)) `shouldBe` Bottom
        divInterval (Interval (Val 10) (Val 20)) (Interval (Val (-5)) (Val 0)) `shouldBe` Bottom

      it "negates intervals correctly" $ do
        negateInterval i1 `shouldBe` Value (Interval (Val (-10)) (Val (-2)))
        negateInterval i3 `shouldBe` Value (Interval (Val (-5)) (Val 5))
        negateInterval i_neg_inf `shouldBe` Value (Interval (Val (-5)) PosInf)
        negateInterval i_pos_inf `shouldBe` Value (Interval NegInf (Val (-5)))
        negateInterval i_inf `shouldBe` Value (Interval NegInf PosInf)

      it "widens intervals correctly" $ do
        wideningInterval i1 i2 `shouldBe` Value (Interval (Val 2) PosInf)
        wideningInterval i2 i1 `shouldBe` Value (Interval NegInf (Val 15))
        wideningInterval i1 (Interval (Val 1) (Val 10)) `shouldBe` Value (Interval NegInf (Val 10))
        wideningInterval i1 (Interval (Val 1) (Val 11)) `shouldBe` Value (Interval NegInf PosInf)

      it "handles less than correctly" $ do
        lessThanInterval i1 i2 `shouldBe` (Value (Interval (Val 2) (Val 10)), Value (Interval (Val 5) (Val 15)))
        lessThanInterval i2 i1 `shouldBe` (Value (Interval (Val 5) (Val 9)), Value (Interval (Val 6) (Val 10)))
        lessThanInterval i1 (Interval (Val 11) (Val 12)) `shouldBe` (Value (Interval (Val 2) (Val 10)), Value (Interval (Val 11) (Val 12)))
        lessThanInterval (Interval (Val 11) (Val 12)) i1 `shouldBe` (Bottom, Bottom)

      it "handles less than or equal correctly" $ do
        lessThanEqualInterval i1 i2 `shouldBe` (Value (Interval (Val 2) (Val 10)), Value (Interval (Val 5) (Val 15)))
        lessThanEqualInterval i2 i1 `shouldBe` (Value (Interval (Val 5) (Val 10)), Value (Interval (Val 5) (Val 10)))
        lessThanEqualInterval i1 (Interval (Val 10) (Val 12)) `shouldBe` (Value (Interval (Val 2) (Val 10)), Value (Interval (Val 10) (Val 12)))
        lessThanEqualInterval (Interval (Val 11) (Val 12)) i1 `shouldBe` (Bottom, Bottom)

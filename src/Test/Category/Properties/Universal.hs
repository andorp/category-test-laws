module Test.Category.Properties.Universal where

import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Category.Hask.Universal
import qualified Control.Category.Hask.Laws.Universal as Laws

productCommutesLeftWith eq gen_fca gen_fcb gen_c _type =
  forAll gen_fca $ \(Fun _ fca) ->
  forAll gen_fcb $ \(Fun _ fcb) ->
  forAll gen_c   $ \c ->
  Laws.productCommutesLeftWith eq fca fcb c _type

productCommutesRightWith eq gen_fca gen_fcb gen_c _type =
  forAll gen_fca $ \(Fun _ fca) ->
  forAll gen_fcb $ \(Fun _ fcb) ->
  forAll gen_c   $ \c ->
  Laws.productCommutesRightWith eq fca fcb c _type

coProductCommutesLeftWith eq gen_fac gen_fbc gen_a _type =
  forAll gen_fac $ \(Fun _ fac) ->
  forAll gen_fbc $ \(Fun _ fbc) ->
  forAll gen_a   $ \a ->
  Laws.coProductCommutesLeftWith eq fac fbc a _type

coProductCommutesRightWith eq gen_fac gen_fbc gen_b _type =
  forAll gen_fac $ \(Fun _ fac) ->
  forAll gen_fbc $ \(Fun _ fbc) ->
  forAll gen_b   $ \b ->
  Laws.coProductCommutesRightWith eq fac fbc b _type

productCommutesLeft
  :: (Product prod, Eq a, Show a, Show b, Show c)
  => Gen (Fun c a) -> Gen (Fun c b) -> Gen c -> prod a b
  -> Property
productCommutesLeft = productCommutesLeftWith (==)

productCommutesRight
  :: (Product prod, Eq b, Show a, Show b, Show c)
  => Gen (Fun c a) -> Gen (Fun c b) -> Gen c -> prod a b
  -> Property
productCommutesRight = productCommutesRightWith (==)

coProductCommutesLeft
  :: (CoProduct coProd, Eq c, Show a, Show b, Show c)
  => Gen (Fun a c) -> Gen (Fun b c) -> Gen a -> coProd a b
  -> Property
coProductCommutesLeft = coProductCommutesLeftWith (==)

coProductCommutesRight
  :: (CoProduct coProd, Eq c, Show a, Show b, Show c)
  => Gen (Fun a c) -> Gen (Fun b c) -> Gen b -> coProd a b
  -> Property
coProductCommutesRight = coProductCommutesRightWith (==)

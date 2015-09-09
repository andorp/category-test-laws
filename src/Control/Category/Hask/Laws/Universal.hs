{-# LANGUAGE Rank2Types #-}
module Control.Category.Hask.Laws.Universal (
    productCommutesLeft
  , productCommutesRight
  , productCommutesLeftWith
  , productCommutesRightWith
  , coProductCommutesLeft
  , coProductCommutesRight
  , coProductCommutesLeftWith
  , coProductCommutesRightWith
  ) where

import Prelude hiding (product)
import Control.Category.Hask.Universal

data ProductT p = ProductT {
    productT   :: forall a b c . (c -> a) -> (c -> b) -> c -> p a b
  , projLeftT  :: forall a b . p a b -> a
  , projRightT :: forall a b . p a b -> b
  }

mkProductT :: (Product prod) => prod a b -> ProductT prod
mkProductT _type = ProductT product projLeft projRight

productCommutesLeftTWith eq f g c p  = f c `eq` (projLeftT p (productT p f g c))
productCommutesRightTWith eq f g c p  = g c `eq` (projRightT p (productT p f g c))

productCommutesLeftWith :: (Product prod) => (a -> a -> Bool) -> (c -> a) -> (c -> b) -> c -> prod a b -> Bool
productCommutesLeftWith eq f g a _type = productCommutesLeftTWith eq f g a (mkProductT _type)

productCommutesRightWith :: (Product prod) => (b -> b -> Bool) -> (c -> a) -> (c -> b) -> c -> prod a b -> Bool
productCommutesRightWith eq f g a _type = productCommutesRightTWith eq f g a (mkProductT _type)

productCommutesLeft :: (Product prod, Eq a) => (c -> a) -> (c -> b) -> c -> prod a b -> Bool
productCommutesLeft = productCommutesLeftWith (==)

productCommutesRight :: (Product prod, Eq b) => (c -> a) -> (c -> b) -> c -> prod a b -> Bool
productCommutesRight = productCommutesRightWith (==)

-- Type hack
data CoProductT cp = CoProductT {
    coProductT :: forall a b c . (a -> c) -> (b -> c) -> cp a b -> c
  , inclLeftT  :: forall a b . a -> cp a b
  , inclRightT :: forall a b . b -> cp a b
  }

mkCoProductT :: (CoProduct coProd) => coProd a b -> CoProductT coProd
mkCoProductT _type = CoProductT coProduct inclLeft inclRight

coProductCommutesLeftTWith eq f g a cp  = f a `eq` (coProductT cp f g (inclLeftT cp a))
coProductCommutesRightTWith eq f g b cp = g b `eq` (coProductT cp f g (inclRightT cp b))

coProductCommutesLeftWith :: (CoProduct coProd) => (c -> c -> Bool) -> (a -> c) -> (b -> c) -> a -> coProd a b -> Bool
coProductCommutesLeftWith eq f g a _type = coProductCommutesLeftTWith eq f g a (mkCoProductT _type)

coProductCommutesRightWith :: (CoProduct coProd) => (c -> c -> Bool) -> (a -> c) -> (b -> c) -> b -> coProd a b -> Bool
coProductCommutesRightWith eq f g a _type = coProductCommutesRightTWith eq f g a (mkCoProductT _type)

coProductCommutesLeft :: (CoProduct coProd, Eq c) => (a -> c) -> (b -> c) -> a -> coProd a b -> Bool
coProductCommutesLeft = coProductCommutesLeftWith (==)

coProductCommutesRight :: (CoProduct coProd, Eq c) => (a -> c) -> (b -> c) -> b -> coProd a b -> Bool
coProductCommutesRight = coProductCommutesRightWith (==)








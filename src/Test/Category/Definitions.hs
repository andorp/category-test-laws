{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Test.Category.Definitions where

import Control.Applicative
import Control.Monad (ap)
import Test.Category.Hask (FC(..))


-- Natural Transformation
type NatTrans f g = (Functor f, Functor g) => forall a . f a -> g a


class (Functor f, Functor g) => Adjoint f g | f -> g , g -> f where
  unit   :: a -> g (f a)
  counit :: f (g a) -> a

instance Adjoint f g => Monad (FC g f) where
  return  = FC . unit
  x >>= f = FC . fmap counit . unFC $ fmap (unFC . f) x

instance Adjoint f g => Applicative (FC g f) where
  pure  = return
  (<*>) = ap

-- * Adjoint example

instance Adjoint ((,) a) ((->) a) where
  -- unit :: b -> (a -> (a,b))
  unit x = \y -> (y, x)
  -- counit :: (a,a -> b) -> b
  counit (x, f) = f x

-- Now this is isomorp to the state monad
type State s = FC ((->) s) ((,) s)


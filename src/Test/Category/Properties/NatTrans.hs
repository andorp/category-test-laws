{-# LANGUAGE Rank2Types #-}
module Test.Category.Properties.NatTrans where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified Test.Category.Laws.NatTrans as Laws
import Test.Category.Definitions (NatTrans(..))

naturalityWith
  :: (Functor f, Functor g, Show a, Show b, Show (f a))
  => Gen (Fun a b) -> Gen (f a) -> (g b -> g b -> Bool) -> NatTrans f g
  -> Property
naturalityWith gen_fab gen_a eq natTrans =
  forAll gen_fab $ \(Fun _ fab) ->
  forAll gen_a   $ \a ->
  Laws.naturalityWith eq natTrans fab a

naturality
  :: (Show (f a), Show b, Show a, Functor g, Functor f, Eq (g b))
  => Gen (Fun a b) -> Gen (f a) -> NatTrans f g
  -> Property
naturality gen_fab gen_a natTrans =
  naturalityWith gen_fab gen_a (==) natTrans
module Test.Category.Properties.Functor where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified Test.Category.Laws.Functor as Laws

identity gen_fa eq = forAll gen_fa (Laws.identityWith eq)

composition gen_a gen_fbc gen_fab eq
  = forAll gen_a   $ \x ->
    forAll gen_fab $ \(Fun _ fab) ->
    forAll gen_fbc $ \(Fun _ fbc) ->
    Laws.compositionWith eq fbc fab x



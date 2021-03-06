module Test.Category.Properties.Functor where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified Control.Category.Hask.Laws.Functor as Laws

identityWith gen_fa eq = forAll gen_fa (Laws.identityWith eq)

compositionWith gen_a gen_fbc gen_fab eq
  = forAll gen_a   $ \x ->
    forAll gen_fab $ \(Fun _ fab) ->
    forAll gen_fbc $ \(Fun _ fbc) ->
    Laws.compositionWith eq fbc fab x

composition gen_a gen_fbc gen_fab = compositionWith gen_a gen_fbc gen_fab (==)

identity gen_fa = identityWith gen_fa (==)
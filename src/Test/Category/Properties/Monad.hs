module Test.Category.Properties.Monad where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified Test.Category.Laws.Monad as Laws

etaNaturality gen_fab gen_a eq =
  forAll gen_fab $ \(Fun _ fab) ->
  forAll gen_a   $ \a ->
  Laws.etaNaturalityWith eq fab a

muNaturality gen_fab gen_mma eq =
  forAll gen_fab $ \(Fun _ fab) ->
  forAll gen_mma $ \mma ->
  Laws.muNaturalityWith eq fab mma

law1 gen_mmma eq =
  forAll gen_mmma $ Laws.law1With eq

law2 gen_ma eq =
  forAll gen_ma $ Laws.law2With eq

law3 gen_ma eq =
  forAll gen_ma $ Laws.law3With eq

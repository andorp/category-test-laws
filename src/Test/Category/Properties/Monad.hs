module Test.Category.Properties.Monad where

import Test.QuickCheck
import Test.QuickCheck.Function
import qualified Control.Category.Hask.Laws.Monad as Laws

etaNaturalityWith gen_fab gen_a eq =
  forAll gen_fab $ \(Fun _ fab) ->
  forAll gen_a   $ \a ->
  Laws.etaNaturalityWith eq fab a

muNaturalityWith gen_fab gen_mma eq =
  forAll gen_fab $ \(Fun _ fab) ->
  forAll gen_mma $ \mma ->
  Laws.muNaturalityWith eq fab mma

law1With gen_mmma eq =
  forAll gen_mmma $ Laws.law1With eq

law2With gen_ma eq =
  forAll gen_ma $ Laws.law2With eq

law3With gen_ma eq =
  forAll gen_ma $ Laws.law3With eq

muNaturality gen_fab gen_mma =
  muNaturalityWith gen_fab gen_mma (==)

law1 gen_mmma =
  law1With gen_mmma (==)

law2 gen_ma =
  law2With gen_ma (==)

law3 gen_ma =
  law3With gen_ma (==)

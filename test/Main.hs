module Main where

import qualified Test.Category.Properties.Functor as Functor
import qualified Test.Category.Properties.NatTrans as NatTrans
import qualified Test.Category.Properties.Monad as Monad

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Function

-- Test if the laws are well formated with a well-known structure.

main =
  defaultMain $ testGroup "" [
      testGroup "Functor" [
          testProperty "Identity" $ Functor.identity int_list
        , testProperty "Composition" $ Functor.composition int_list char_to_int int_to_char
        ]
    , testGroup "Natural Transformation" [
          testProperty "Naturality of reverse" $ NatTrans.naturality int_to_char int_list reverse
        ]
    , testGroup "Monad" [
          testProperty "Eta naturality" $ Monad.etaNaturalityWith int_to_char int char_list_eq
        , testProperty "Mu naturality" $ Monad.muNaturality int_to_char int_list_2
        , testProperty "Law1" $ Monad.law1 int_list_3
        , testProperty "Law2" $ Monad.law2 int_list
        , testProperty "Law3" $ Monad.law3 int_list
        ]
    ]

-- * Generators

int :: Gen Int
int = arbitrary

int_list :: Gen [Int]
int_list = arbitrary

int_list_2 :: Gen [[Int]]
int_list_2 = arbitrary

int_list_3 :: Gen [[[Int]]]
int_list_3 = arbitrary

int_to_char :: Gen (Fun Int Char)
int_to_char = arbitrary

char_to_int :: Gen (Fun Char Int)
char_to_int = arbitrary

char_list_eq :: [Char] -> [Char] -> Bool
char_list_eq = (==)

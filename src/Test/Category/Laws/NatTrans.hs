{-# LANGUAGE Rank2Types #-}
module Test.Category.Laws.NatTrans where

import Test.Category.Definitions (NatTrans(..))


naturalityWith :: (Functor f, Functor g) => (g b -> g b -> Bool) -> NatTrans f g -> (a -> b) -> f a -> Bool
naturalityWith eq nt h x = nt (fmap_F h x) `eq` fmap_G h (nt x) where
  fmap_F = fmap
  fmap_G = fmap

naturality :: (Functor f, Functor g, Eq (g b)) => NatTrans f g -> (a -> b) -> f a -> Bool
naturality = naturalityWith (==)

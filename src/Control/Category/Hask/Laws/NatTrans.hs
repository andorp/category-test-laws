{-# LANGUAGE Rank2Types #-}
module Control.Category.Hask.Laws.NatTrans where

import Control.Category.Hask.Definition (NatTrans(..))


naturalityWith :: (Functor f, Functor g) => (g b -> g b -> Bool) -> NatTrans f g -> (a -> b) -> f a -> Bool
naturalityWith eq nt h x = nt (fmap_F h x) `eq` fmap_G h (nt x) where
  fmap_F = fmap
  fmap_G = fmap

naturality :: (Functor f, Functor g, Eq (g b)) => NatTrans f g -> (a -> b) -> f a -> Bool
naturality = naturalityWith (==)

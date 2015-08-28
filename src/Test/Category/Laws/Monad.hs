module Test.Category.Laws.Monad where

import Control.Monad (join)

import Test.Category.Hask
import Test.Category.Laws.NatTrans

-- Monad is a functor equipped with two natural transformations

etaNaturalityWith :: (Functor m, Monad m) => (m b -> m b -> Bool) -> (a -> b) -> a -> Bool
etaNaturalityWith eq f x = naturalityWith eq (eta . unHask) f (Hask x) where
  eta = return

muNaturalityWith :: (Functor m, Monad m) => (m b -> m b -> Bool) -> (a -> b) -> m (m a) -> Bool
muNaturalityWith eq f x = naturalityWith eq (mu . unF2) f (F2 x)  where
  mu = join

law1With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m (m (m a)) -> Bool
law1With eq mmmx = mu (mu mmmx) `eq` mu (fmap mu mmmx) where
  mu = join

law2With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m a -> Bool
law2With eq mx = mu (eta mx) `eq` mx where
  mu  = join
  eta = return

law3With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m a -> Bool
law3With eq mx = mu (fmap eta mx) `eq` mx where
  mu  = join
  eta = return

muNaturality :: (Functor m, Monad m, Eq (m b)) => (a -> b) -> m (m a) -> Bool
muNaturality f x = naturalityWith (==) (mu . unF2) f (F2 x)  where
  mu = join

law1 :: (Functor m, Monad m, Eq (m a)) => m (m (m a)) -> Bool
law1 = law1With (==)

law2 :: (Functor m, Monad m, Eq (m a)) => m a -> Bool
law2 = law2With (==)

law3 :: (Functor m, Monad m, Eq (m a)) => m a -> Bool
law3 = law3With (==)

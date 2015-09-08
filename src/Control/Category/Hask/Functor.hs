module Control.Category.Hask.Functor where

-- Syntactic representation of the Hask category, as
-- it is hidden in many definitions, but we need to have
-- to be able to check some properties

newtype Hask a = Hask { unHask :: a }
-- NOTE: Type `Hask a` is isomorphic to `a`:
-- Hask   :: a -> Hask a
-- unHask :: Hask a -> a

instance Functor Hask where
  fmap f (Hask x) = Hask (f x)

-- For some laws we need to apply the same functor twice

newtype F2 f a = F2 { unF2 :: f (f a) }

instance Functor f => Functor (F2 f) where
  fmap f (F2 ffx) = F2 (fmap (fmap f) ffx)


-- Composition of functors

newtype FC f g a = FC { unFC :: f (g a) }

instance (Functor f, Functor g) => Functor (FC f g) where
  fmap f (FC fgx) = FC (fmap (fmap f) fgx)


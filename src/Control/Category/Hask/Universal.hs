{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Control.Category.Hask.Universal where

-- Universal constructions/properties in the Hask category

-- NOTE: The uniqueness of this function can not be tested via the quickcheck


-- The initial object of the Hask is a type without any value
data Initial

-- There exist only one morphism from the initial object to any other object
uniqueInitial :: Initial -> a
uniqueInitial = \case


-- The final object of the Hask is a type with only one value
data Final = Final
  deriving (Eq, Show)

-- There exist only one mprphishm from any object a to a final object
-- which is a constant function.
uniqueFinal :: a -> Final
uniqueFinal _ = Final


class Product (prod :: * -> * -> *) where
  projLeft  :: prod a b -> a
  projRight :: prod a b -> b
  product   :: (c -> a) -> (c -> b) -> c -> prod a b

instance Product (,) where
  projLeft      = fst
  projRight     = snd
  product f g c = (f c, g c)


class CoProduct (coProd :: * -> * -> *) where
  inclLeft  :: a -> coProd a b
  inclRight :: b -> coProd a b
  coProduct :: (a -> c) -> (b -> c) -> coProd a b -> c

instance CoProduct Either where
  inclLeft  = Left
  inclRight = Right
  coProduct = either


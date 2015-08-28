module Test.Category.Laws.Functor where


identityWith :: (Functor f) => (f a -> f a -> Bool) -> f a -> Bool
identityWith eq fx = id fx `eq` (fmap id fx)

compositionWith :: (Functor f) => (f c -> f c -> Bool) -> (b -> c) -> (a -> b) -> f a -> Bool
compositionWith eq f g fx = (fmap (f . g) fx) `eq` ((fmap f) . (fmap g) $ fx)

identity :: (Functor f, Eq (f a)) => f a -> Bool
identity = identityWith (==)

composition :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
composition = compositionWith (==)


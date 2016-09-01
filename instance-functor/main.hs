-- Function of one argument
data Foo a b = Foo (a -> b)

-- Apply the function to an argument
apply :: Foo a b -> a -> b
apply (Foo f) = f

instance Functor (Foo a) where
  fmap f (Foo g) = Foo (f . g)

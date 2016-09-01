-- Function of one argument
data Foo b a = Foo (a -> b)

-- Apply the function to an argument
apply :: Foo b a -> a -> b
apply (Foo f) = f

instance Functor (Foo b) where
  fmap f (Foo g) = Foo (f . g)

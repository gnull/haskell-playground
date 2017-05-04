{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

toMaybe :: a -> Maybe b -> Maybe (a, b)
toMaybe x (Just y) = Just (x, y)
toMaybe _ Nothing  = Nothing

toMaybeWrong :: a -> Maybe () -> Maybe (a, ())
toMaybeWrong x _ = Just (x, ())

toMaybeFixedA :: Int -> Maybe b -> Maybe (Int, b)
toMaybeFixedA _ (Just b) = Just (1, b)
toMaybeFixedA _ Nothing  = Nothing

kek :: (forall a b . a -> Maybe b -> Maybe (a, b)) -> c -> Maybe d -> Maybe (c, d)
kek g a b = g a b

-- Tests. These should successfully compile.
a = kek toMaybe
b = kek toMaybeFixedA
-- This one shouldn't compile
-- c = kek toMaybeWrong

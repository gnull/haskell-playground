{-# LANGUAGE RankNTypes #-}

toMaybe :: a -> Maybe b -> Maybe (a, b)
toMaybe x (Just y) = Just (x, y)
toMaybe _ Nothing  = Nothing

toMaybeWrong :: a -> Maybe () -> Maybe (a, ())
toMaybeWrong x _ = Just (x, ())

toMaybeFixedA :: Int -> Maybe b -> Maybe (Int, b)
toMaybeFixedA _ (Just b) = Just (1, b)
toMaybeFixedA _ Nothing  = Nothing

kek :: (forall b . a -> Maybe b -> Maybe (a, b)) -> a -> Maybe d -> Maybe (a, d)
kek g a b = g a b

-- Looks like this should be as safe as `kek`
kak :: (forall b . a -> Maybe b -> Maybe (a, b)) -> a -> Maybe () -> Maybe (a, ())
kak = kek

-- Tests. These should successfully compile.
a = kek toMaybe
b = kek toMaybeFixedA
a' = kak toMaybe
b' = kak toMaybeFixedA
-- This one shouldn't compile
-- c = kak toMaybeWrong
-- type this in REPL to get error:
--   :t kak toMaybeWrong
--   :t kek toMaybeWrong

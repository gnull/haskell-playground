import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Data.Functor (Functor, fmap)
import Data.List (genericLength, groupBy, sortBy)
import Data.Ord (comparing)

-- Discrete random value distribution specified by list of its values with
-- probabilities
--
-- We could add a requirement for x to be an instance of Eq, so we could merge
-- elements of rand with the same x'es: replace elements (p1, x) and (p2, x)
-- with (p1 + p2, x). But it would not allow us to store functions in x'es. To
-- solve this I've written function merge, that can be called manually for
-- random values that are instances of Eq.
data Rand p x = Rand [(p, x)] deriving (Show)

unRand (Rand x) = x

instance Fractional p => Functor (Rand p) where
  fmap f (Rand xs) = Rand $ fmap (\(p, x) -> (p, f x)) xs

instance Fractional p => Applicative (Rand p) where
  pure x = Rand $ pure (1, x)
  (Rand fs) <*> (Rand xs) = Rand $ fun <$> fs <*> xs where
    fun (pf, f) (px, x) = (pf * px, f x)

uniformly :: (Fractional p) => [a] -> Rand p a
uniformly xs = Rand $ fmap (\x -> (1 / genericLength xs, x)) xs

-- This function is useful only for pretty-printing random value distribution.
-- It merges multiple duplicate values of the distribution into one. Duplicates
-- are ok for calculations since they do not affect the result.
merge :: (Fractional p, Eq x, Ord x) => Rand p x -> Rand p x
merge = Rand . fn . unRand where
    folder (px, x) (py, y) = (px + py, x)
    fn = map (foldl1 folder) . groupBy (\x y -> snd x == snd y) . sortBy (comparing snd)

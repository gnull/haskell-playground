import System.Environment (getArgs)

import Control.Arrow ((&&&))
import Data.List (group, sort, insertBy, sortBy)
import Data.Ord (comparing)

type Bit = Int

data HTree a = Leaf a | Branch (HTree a) (HTree a)
  deriving (Show)

foldHTree :: [(Int, HTree a)] -> HTree a
foldHTree ((_, x):[]) = x
foldHTree ((cx, x):(cy, y):rest) = foldHTree $ insertBy (comparing fst) union rest where
  union = (cx + cy, Branch x y)

toCode :: HTree a -> [(a, [Bit])]
toCode (Leaf x) = [(x, [])]
toCode (Branch l r) = append' 1 (toCode l) ++ append' 0 (toCode r) where
  append' x = map (\(s, code) -> (s, x:code))

freq :: Ord a => [a] -> [(Int, a)]
freq = map (length &&& head) . group . sort

huffman :: (Ord a) => [a] -> [(a, [Bit])]
huffman = toCode . foldHTree . map (\(c, a) -> (c, Leaf a)) . sortBy (comparing fst) . freq

-- ########

showBits = concat . (map show)

showCode :: (Show a) => (a, [Bit]) -> String
showCode (c, bits) = show c ++ ": " ++ showBits bits

main = do
  inputFile:codeFile:outputFile:_ <- getArgs
  contents <- readFile inputFile
  let code = huffman contents
  writeFile codeFile $ unlines $ map showCode code
  writeFile outputFile $
    concat $ map showBits $
    map (\x -> let (Just y) = lookup x code in y) contents

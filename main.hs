import System.Environment
import Text.Read

eval' :: [String] -> [Integer]
eval' xs = foldl f [] xs
  where
    f acc x = case (acc, x) of
      (a:b:rest, "+") -> (b + a):rest
      (a:b:rest, "-") -> (b - a):rest
      (rest, op) | op `elem` ["+", "-"]
                   -> error $ "Trying to apply " ++ show op ++ " to stack " ++
                      show rest ++ " that doesn't have enough elements"
      (rest, num) -> case readMaybe num of
                       Just a -> a:rest
                       Nothing -> error $ show num ++ " doesn't seem to be either an operator or a number"

eval = eval' . words

main = do
  eq <- getArgs
  putStrLn $ show $ eval $ unwords $ eq

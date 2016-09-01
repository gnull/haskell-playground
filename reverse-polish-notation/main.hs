import System.Environment (getArgs)
import Text.Read (readMaybe)

type Value = Integer

data Token = BinaryOperator (Value -> Value -> Value) String | Number Value

instance Show Token where
  show (BinaryOperator f s) = show s
  show (Number n) = show n

toToken :: String -> Token
toToken op@("+") = BinaryOperator (+) op
toToken op@("-") = BinaryOperator (-) op
toToken op@("div") = BinaryOperator div op
toToken x = case readMaybe x of
              Just i -> Number i
              Nothing -> error $ show x ++ " is neither operator nor number"

eval' :: [Token] -> [Value]
eval' xs = foldl f [] xs
  where
    f acc x = case (acc, x) of
      (rest, Number n) -> n:rest
      (a:b:rest, BinaryOperator f _) -> f b a : rest
      (_, op@(BinaryOperator f _))   ->
        error $ "Trying to apply " ++ show op ++
              " while the stack " ++ show acc ++ " doesn't have enough elements"

main = getArgs >>= print . eval' . map toToken . (>>= words)


import System.Environment (getArgs)
import Control.Arrow (first)

type Value = Integer

-- data Op

data Op = Plus | Minus | Div

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Div   = "div"

instance Read Op where
  readsPrec d ('+':s) = [(Plus,  s)]
  readsPrec d ('-':s) = [(Minus, s)]
  readsPrec d ('d':'i':'v':s) = [(Div, s)]
  readsPrec d _ = []

opFun :: Integral n => Op -> (n -> n -> n)
opFun Plus  = (+)
opFun Minus = (-)
opFun Div   = div

-- data Token

data Token = BinaryOperator Op | Number Value

instance Show Token where
  show (BinaryOperator o) = show o
  show (Number n) = show n

instance Read Token where
  readsPrec d s =
    map (first BinaryOperator) (readsPrec d s :: [(Op, String)]) ++
    map (first Number)         (readsPrec d s :: [(Value, String)])

-- execution

eval :: [Token] -> [Value]
eval = foldl f []
  where
    f st (Number n) = n : st
    f (a : b : st) (BinaryOperator f) = opFun f b a : st
    f st o = error $ "Trying to apply (" ++ show o ++ ") while the stack " ++
               show st ++ " doesn't have enough elements"

main = getArgs >>= print . eval . map read . (>>= words)


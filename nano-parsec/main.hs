-- Example from tutorial at dev.stephendiehl.com

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser a s = case parse a s of
  [(x, [])] -> x
  [(_, rest)] -> error $ "Parse error at " ++ show rest
  _ -> error "Parse error"

item :: Parser Char
item = Parser $ \s -> case s of
  [] -> []
  x:xs -> [(x, xs)]

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $
  \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

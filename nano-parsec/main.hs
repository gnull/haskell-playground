-- Example from tutorial at dev.stephendiehl.com
import Data.Functor (Functor)
import Control.Applicative (Applicative, pure, (<*>))

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

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, s') | (a, s') <- p s]

instance Applicative Parser where
  pure = unit
  (Parser p1) <*> (Parser p2) = Parser $ \s ->
    do
      (f, s1) <- p1 s
      (a, s2) <- p2 s1
      return (f a, s2)

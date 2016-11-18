-- Example from tutorial at dev.stephendiehl.com
import Data.Functor (Functor, (<$>))
import Control.Applicative (Applicative, pure, (<*>), Alternative, empty, (<|>), some, many)
import Control.Monad (Monad, (>>=), return, MonadPlus, mzero, mplus)

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

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, s') | (a, s') <- p s]

instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) = Parser $ \s ->
    do
      (f, s1) <- p1 s
      (a, s2) <- p2 s1
      return (f a, s2)

instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f = Parser $
    \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ \s ->
    case parse p s of
      [] -> parse q s
      x  -> x

-- Some funny higher-order functions and Parser's

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c then return c else empty

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa pf =
  do {a <- pa; rest a} where
    rest a =
      do
        f <- pf
        b <- pa
        rest $ f a b
      <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl pa pf a = chainl1 pa pf <|> return a

char :: Char -> Parser Char
char x = satisfy (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  x  <- char x
  xs <- string xs
  return $ x:xs

digit :: Parser Char
digit = oneOf ['0'..'9']

number :: Parser Integer
number = do
  sign   <- string "-" <|> return []
  digits <- some digit
  return $ read $ sign ++ digits

parens :: Char -> Char -> Parser a -> Parser a
parens open close p =
  do
    char open
    x <- p
    char close
    return x

-- #######

data Node = Node String [Node]

instance Show Node where
  show (Node name children) =
    unlines $ (name:) $ map ("  " ++) $ concat $ map (lines . show) children

nodeName :: Parser String
nodeName = many $ oneOf $ ['a'..'z'] ++ "_-"

space :: Parser ()
space = do {oneOf " \t\b\n\r"; return ()}

-- The chainl functions below should be rewritten to avoid the bullshit above
sepList :: Parser b -> Parser a -> Parser [a]
sepList sep val = do
  chainl val' sep' [] where
    sep' = do
      sep
      return (++)
    val' = do
      x <- val
      return [x]

node :: Parser Node
node = do
  name <- nodeName
  many space
  res <- parens '{' '}' $ do
    many space
    res <- sepList (many space) node
    many space
    return res
  many space;
  char ';'
  return $ Node name res

main = do
  interact $ show . runParser p' where
    p' = do
      many space
      x <- node
      many space
      return x

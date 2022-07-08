module Regex where

import Control.Applicative

data Parser a = P (String -> Maybe (a,String))
type Pattern = String
type Machine = Parser String

parse :: Parser a -> String -> Maybe (a, String)
parse (P f) st = f st

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser =  pure f <*> parser

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\xs -> Just (x,xs))

  -- <*> :: (Parser (a->b)) -> Parser a -> Parser b
  (<*>) pf px = P (\s ->
    case parse pf s of
      Nothing -> empty
      Just (f, s') -> 
        case parse px s' of
        Nothing       -> empty
        Just (x, s'') -> Just (f x, s''))

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \cs ->
    case parse p cs of
      Nothing -> parse q cs
      mx      -> mx

-- GENERIC PARSERS
-- parse a char c when P c.
sat :: (Char -> Bool) -> Parser Char
sat p = P $ foo
  where
    foo (c:cs) = if p c then Just $ (c, cs) else Nothing
    foo _ = Nothing                                     

-- parse one character                                 
item :: Parser Char
item = sat $ const True

digit :: Parser Char
digit = sat (\x -> elem x ['0'..'9'])

-- parse the character x
char :: Char -> Parser Char
char x = sat (== x)

-- parse the string xs
string :: String -> Parser String
string [] = pure []
string (x:xs) = liftA2 (:) (char x) (string xs)

-- parse a natural number
nat :: Parser Integer
nat = read <$> (some digit)


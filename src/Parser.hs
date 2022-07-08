module Parser where

import Regex
import Control.Applicative

-- FSM STUFF
perhaps :: Machine -> Machine
perhaps x = x <|> nullmachine

nullmachine :: Machine
nullmachine = P (\cs -> Just ("",cs))

append :: Machine -> Machine -> Machine
append p q = fmap (++) p <*> q

star :: Machine -> Machine
star f = append f (star f) <|> f <|> pure ""

plus :: Machine -> Machine
plus f = append f $ star f

charmatch :: String -> Machine
charmatch xs = P $ \cs ->
  head $
    [ Nothing | cs == ""] ++
    [ Just ([head cs], tail cs) | head cs `elem` xs] ++
    [ Nothing ]

anychar :: Machine
anychar = (:[]) <$> item


-- PARSER MACHINES
isspecial :: Char -> Bool
isspecial c = elem c "\\, [, ], *, +, {, }, (, ), ."

mundanechar :: Parser Char
mundanechar = sat (not.isspecial)

specialchar :: Parser Char
specialchar = char '\\' *> sat isspecial

regularchar :: Parser Char
regularchar = mundanechar <|> specialchar

symbolset :: Parser Machine
symbolset = charmatch <$> (char '[' *> some regularchar <* char ']')

uptontimes :: Integer -> Machine -> Machine
uptontimes 0 _ = nullmachine
uptontimes n x = (uptontimes (n-1) x) `append` (perhaps x)

ntimes :: Integer -> Machine -> Machine
ntimes 0 _ = nullmachine
ntimes n m = m `append` (ntimes (n-1) m)

nreps :: Parser Machine
nreps = regex <**> (ntimes <$> (char '{' *> nat <* char '}'))

ntomtimes :: (Integer, Integer) -> Machine -> Machine
ntomtimes (n,m) x = (ntimes n x) `append` (uptontimes (m-n) x)

ntomreps :: Parser Machine
ntomreps = atom <**> (ntomtimes <$> getintegers)
  where
    getintegers :: Parser (Integer, Integer)
    getintegers = liftA2 (,) (char '{' *> nat <* char ',') (nat <* char '}')


-- Glue
atoms :: [Parser Machine]
atoms = [symbolset, 
         (charmatch.(:[])) <$> regularchar,
         char '.' *> pure anychar,
         char '(' *> regex <* char ')']

atom :: Parser Machine
atom  = foldr (<|>) empty atoms

regeces :: [Parser Machine]
regeces = liftA2 (liftA2 append) 
             [(star <$> (atom <* char '*')),
              (plus <$> (atom <* char '+')),
              ntomreps,
              atom] [regex] ++
              [pure nullmachine]

--regeces :: [Parser Machine]
--regeces = [liftA2 append (star <$> (atom <* char '*')) regex,
--           liftA2 append (plus <$> (atom <* char '+')) regex,
--           liftA2 append ntomreps regex,
--           liftA2 append atom regex,
--           pure nullmachine]

regex :: Parser Machine
regex = foldr (<|>) empty regeces

applyh :: Pattern -> String -> (Maybe (Maybe (String, String)))
applyh patt input = (parse.fst <$> parse regex patt) <*> pure input

apply :: Pattern -> String -> Bool
apply patt input = case applyh patt input of
  Just (Just (_,"")) -> True
  _                  -> False

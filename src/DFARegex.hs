module DFARegex where

import Control.Applicative

data ThompsonNFA st = TNFA {
              states :: [st], -- Finite list
	      alphabet :: [Maybe Char], -- Set
	      transition :: st -> Char -> st,
	      init :: Maybe st, -- Element of states
	      final :: Maybe st -- Element of states
	      }

evalNFA :: ThompsonNFA st -> String -> Bool
evalNFA = undefined

-- accepts only the empty string
emptynfa :: ThompsonNFA st
emptynfa = TNFA [] [] const Nothing Nothing

instance ThompsonNFA Semigroup where
  --(<>) :: ThompsonNFA -> ThompsonNFA -> ThompsonNFA
  (<>) = undefined

instance ThompsonNFA Monoid where
  mempty = emptynfa

instance Alternative ThompsonNFA where
  empty = emptynfa

  (<|>) = undefined


module FPParser where

import Char (isAlpha)

infixl 3 <*>, <*, *>
infixl 4 <$>, <|>

type Parser a = String -> [(a,String)]

parse :: Parser a -> String -> Maybe a
parse p s = case filter (null . snd) $ p s of
              [(x,_)] -> Just x
              _         -> Nothing

failure :: Parser a
failure = const []

empty :: Parser ()
empty = yield ()

yield :: a -> Parser a
yield x = \str -> [(x,str)]

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
pF <*> pA = \str -> [(f valA,str'') | (f,str') <- pF str, (valA, str'') <- pA str']

(<*) :: Parser a -> Parser b -> Parser a
pA <* pB = const <$> pA <*> pB

(*>) :: Parser a -> Parser b -> Parser b
pA *> pB = const id <$> pA <*> pB

(<|>) :: Parser a -> Parser a -> Parser a
pA <|> pB = \str -> pA str ++ pB str

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> pA = yield f <*> pA

anyChar :: Parser Char
anyChar = \s -> case s of
                    []     -> []
                    (c:cs) -> [(c,cs)]

check :: (a -> Bool) -> Parser a -> Parser a
check ok p = filter (ok . fst) . p

charPred :: (Char -> Bool) -> Parser Char
charPred p [] = []
charPred p (c:cs) = if p c
                      then [(c,cs)]
                      else []

char :: Char -> Parser Char
char c = check (c==) anyChar

alpha :: Parser String
alpha = some (charPred isAlpha)

number :: Parser Int
number = neg <|> nat
 where
  neg = negate <$> (char '-' *> nat)

nat :: Parser Int
nat = foldl (\acc nat -> acc * 10 + (ord nat) - offset) 0 <$>
  some (charPred (\c -> ord c <= 57 && ord c >= 48))
 where
  offset = ord '0'

word :: String -> Parser String
word []     = yield ""
word (c:cs) = (:) <$> char c <*> word cs

many :: Parser a -> Parser [a]
many p = some p <|> yield []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

-- count :: Int -> Parser a -> Parser [a]
-- -- ^ @count n p@ parses @n@ occurrences of @p@ in sequence. A list of
-- --   results is returned.
-- count n p = sequence (replicate n p)
         
sepBy :: Parser a -> Parser sep -> Parser [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep <|> yield []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = (:) <$> p <*> (many (sep *> p))

between :: Parser a -> Parser b -> Parser c -> Parser c
between pOpen pClose pA = pOpen *> pA <* pClose

sequence :: [Parser a] -> Parser [a]
sequence = traverse id

traverse :: (a -> b) -> [Parser a] -> Parser [b]
traverse _ []       = yield []
traverse f (pA:pAs) = (\x -> (f x :)) <$> pA <*> traverse f pAs
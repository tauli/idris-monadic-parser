module Parser

data Parser s o = P (s -> Either String (o, s))

class Parsable s o where
  item : Parser s o

instance Parsable (List a) a where 
  item = P (\inp => case inp of 
                    Nil => Left "Error! Parsing empty list."
                    (x :: xs) => Right (x, xs)
           )

instance Parsable String Char where
  item = P (\inp => case choose (inp == "") of
                      Left  _ => Left "Error! Parsing empty list."
                      Right p => Right (strHead' inp p, strTail' inp p)
           )

parse : (Parsable s o') => Parser s o -> s -> Either String (o, s)
parse (P p) inp = p inp

instance (Parsable s o) => Functor (Parser s) where
  map f p = P (\inp => case parse p inp of
                          Left err        => Left err
                          Right (v, rest) => Right ((f v), rest))

instance (Parsable s o) => Applicative (Parser s) where
  pure v  = P (\inp => Right (v, inp))
  a <$> b = P (\inp => do (f, rest ) <- parse a inp
                          (x, rest') <- parse b rest
                          pure ((f x), rest'))

instance (Parsable s o) => Monad (Parser s) where
  p >>= f = P (\inp => case parse p inp of
                         Left err       => Left err
                         Right (v,rest) => parse (f v) rest)

instance (Parsable s o) => Alternative (Parser s) where
  empty   = P (const (Left "empty"))
  p <|> q = P (\inp => case parse p inp of
                         Left msg      => parse q inp
                         Right (v,out) => Right (v,out))

failure : String -> Parser s o
failure msg = P (\_ => Left msg)

-- primitive

sat : (Parsable s o) => (o -> Bool) -> Parser s o
sat p = do x <- item
           guard (p x)
           pure x
           --if (p x) then pure x else empty

oneof : (Eq o, Parsable s o) => List o -> Parser s o
oneof xs = sat (\x => elem x xs)

digit : (Parsable s Char) => Parser s Char
digit = sat isDigit

lower : (Parsable s Char) => Parser s Char
lower = sat isLower

upper : (Parsable s Char) => Parser s Char
upper = sat isUpper

letter : (Parsable s Char) => Parser s Char
letter = sat isAlpha

alphanum : (Parsable s Char) => Parser s Char
alphanum = sat isAlphaNum

char : (Parsable s Char) => Char -> Parser s Char
char c = sat (c ==)

string : (Parsable s Char) => String -> Parser s String
string s = map pack (traverse char (unpack s))

-- combinators 

optional : (Parsable s o) => Parser s a -> Parser s (Maybe a)
optional p = map Just p <|> pure Nothing

mutual
  many1 : (Parsable s o) => Parser s a -> Parser s (List a)
  many1 p = [| p :: many p |]

  many : (Parsable s o) => Parser s a -> Parser s (List a)
  many p = lazy (many1 p) <|> pure []

sepBy1 : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
sepBy1 p s = [| p :: many (s $> p) |]

sepBy : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
sepBy p s = sepBy1 p s <|> pure Nil

manyTil : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
manyTil p e = (e $> pure Nil) <|> lazy [| p :: manyTil p e |] <|> pure Nil

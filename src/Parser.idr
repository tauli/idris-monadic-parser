module Parser

%default total

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

infix 0 <?> 
(<?>) : (Parsable s o) => Parser s a -> String -> Parser s a
(<?>) p s = p <|> failure s

-- primitive

sat : (Parsable s o) => (o -> Bool) -> Parser s o
sat p = do x <- item
           if (p x) then pure x else empty

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

charNoCase : (Parsable s Char) => Char -> Parser s Char
charNoCase c = sat ((toUpper c) ==) <|> sat ((toLower c) ==)

string : (Parsable s Char) => String -> Parser s String
string s = map pack (traverse char (unpack s))

stringNoCase : (Parsable s Char) => String -> Parser s String
stringNoCase s = map pack (traverse charNoCase (unpack s))

-- combinators 

optional : (Parsable s o) => Parser s a -> Parser s (Maybe a)
optional p = map Just p <|> pure Nothing

except : (Parsable s o) => Parser s a -> Parser s a -> Parser s a
except p q = P (\inp => case parse q inp of
                        (Left _)  => parse p inp
                        (Right _) => Left "empty"
               )

some : (Parsable s o) => Parser s a -> Nat -> Parser s (List a)
some _  Z    = with List pure Nil
some p (S n) = do
  (Just i) <- optional p
    | Nothing => with List pure Nil
  with List [| pure i :: some p n |]
--some p (S n) = with List [| p :: (some p n) |] <|> with List pure Nil 

some1 : (Parsable s o) => Parser s a -> Nat -> Parser s (List a)
some1 _ Z     = with List pure Nil
some1 p (S n) = with List [| p :: some p n |]

exactly : (Parsable s o) => Parser s a -> Nat -> Parser s (List a)
exactly _ Z     = with List pure Nil
exactly p (S n) = with List [| p :: exactly p n |]

covering
many : (Parsable s o) => Parser s a -> Parser s (List a)
many p = do
  (Just i) <- optional p
    | Nothing => with List pure Nil
  with List [| pure i :: many p |] 

covering
many1 : (Parsable s o) => Parser s a -> Parser s (List a)
many1 p = with List [| p :: many p |]

covering
sepBy1 : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
sepBy1 p s = with List [| p :: many (s $> p) |]

covering
sepBy : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
sepBy p s = with List (sepBy1 p s <|> pure Nil)

covering
manyTil : (Parsable s o) => Parser s a -> Parser s b -> Parser s (List a)
manyTil p e = with List (e $> pure Nil) <|> manyTil' where
  covering
  manyTil' : (Parsable s o) => Parser s (List a)
  manyTil' = do
    i <- optional p
    case i of 
         Nothing => with List pure Nil
         Just a  => with List [| pure a :: manyTil p e |]


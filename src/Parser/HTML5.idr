module Parser.HTML5

import Parser

Attribute : Type
Attribute = (String, String)

data Tag = 
      TagOpen    String (List Attribute)
    | TagClose   String
    | TagText    String
    | TagComment String

instance Show Tag where
  show (TagOpen    s xs) = "TagOpen "    ++ s ++ show xs
  show (TagClose   s   ) = "TagClose "   ++ s
  show (TagText    s   ) = "TagText "    ++ s
  show (TagComment s   ) = "TagComment " ++ s

quoted : (Parsable s Char) => Parser s o -> Parser s o
quoted p = char '\'' $> p <$ char '\'' <|> char '"' $> p <$ char '"'

byteOrderMark : (Parsable s Char) => Parser s Char
byteOrderMark = char '\xFEFF'

htmlSpaceList : List Char
htmlSpaceList = ['\x0009', '\x000A', '\x000C', '\x000D', '\x0020']

htmlSpace : (Parsable s Char) => Parser s Char
htmlSpace = oneof htmlSpaceList

doctype : (Parsable s Char) => Parser s ()
doctype = do
    stringNoCase "<!DOCTYPE"
    many htmlSpace
    stringNoCase "html"
    many htmlSpace
    many $ sat (/= '>')
    char '>'
    return () 

comment : (Parsable s Char) => Parser s Tag
comment = do
    string "<!--"
    c <- manyTil item $ string "-->"
    return $ TagComment $ pack c

cdata : (Parsable s Char) => Parser s Tag
cdata = do
  string "<![CDATA["
  c <- manyTil item $ string "]]>"
  return $ TagText $ pack c

text : (Parsable s Char) => Parser s Tag
--text = map (TagText . pack) $ many1 $ sat (/= '<')
text = do
    t <- many1 $ sat (/= '<')
    return $ (TagText . pack) t

attributeName : (Parsable s Char) => Parser s String
attributeName = map pack $ many1 $ sat (
  \x => not $ elem x (htmlSpaceList ++ ['\x00','\x22','\x27','>','/','='])
  )

unqotedAVal : (Parsable s Char) => Parser s String
unqotedAVal = map pack $ many1 $ sat (
  \x => not $ elem x (htmlSpaceList ++ ['\x22','\x27','=','<','>','\x60'])
  )

singleQuoteAVal : (Parsable s Char) => Parser s String
singleQuoteAVal = 
  char '\x22' $> (map pack $ many $ sat (/= '\x22')) <$ char '\x22'

doubleQuoteAVal : (Parsable s Char) => Parser s String
doubleQuoteAVal = 
  char '\x27' $> (map pack $ many $ sat (/= '\x27')) <$ char '\x27'

attributeValue : (Parsable s Char) => Parser s Attribute
attributeValue = do
    n <- attributeName
    many htmlSpace 
    char '='
    many htmlSpace
    v <- unqotedAVal <|> singleQuoteAVal <|> doubleQuoteAVal
    return (n,v)

attribute : (Parsable s Char) => Parser s Attribute
attribute = attributeValue <|> (map (\x => (x,"")) attributeName) 

tag : (Parsable s Char) => Parser s o -> Parser s o
tag p = char '<' $> p <$ char '>'

startTag : Parsable s Char => Parser s Tag
startTag = do
    n <- map pack $ many1 alphanum
    many htmlSpace
    a <- attribute `sepBy` htmlSpace
    many htmlSpace
    optional $ char '/'
    return $ TagOpen n a

endTag : Parsable s Char => Parser s Tag
endTag = map (TagClose . pack) (char '/' $> many1 alphanum <$ many htmlSpace)

element : Parsable s Char => Parser s Tag
element = comment <|> cdata <|> tag startTag <|> tag endTag <|> text

htmlDoc : Parsable s Char => Parser s (List Tag)
htmlDoc = do
    many htmlSpace
    optional doctype
    many htmlSpace
    element `sepBy` (many htmlSpace) 


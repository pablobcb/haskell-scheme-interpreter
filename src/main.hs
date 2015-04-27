module Main where

import Control.Monad (liftM)
import Data.Array (Array (..), listArray)
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- quoted, complex numbers
main :: IO ()
main = do
    let string    = ["\"10\"", "\"iso \\n \"string\""]
    let number    = ["42.10", "42", "#b101010", "#d234", "#o12345", "#x8F"]
    let character = ["'\n'", "' '", "'a'"]
    let boolean   = ["#t", "#f"]
    let atom      = ["@asd" , "$sddd"]
    let list      = ["(a list)", "(a (nested) list then)", "(a (dotted . list) test)"]
    let quoted    = []

    mapM (putStrLn . readExpr ) (string ++ number ++ boolean ++ character ++ atom ++ list ++ quoted)

    return ()

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Bool Bool 
           | Character Char
           | Float Double
           | Complex (Complex)
           deriving Show


-- generaters a parser that checks if a
-- character exists in !#$%&|*+-/:<=>?@^_~
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


-- generaters a parser that checks if next
-- character is ' ' and skips it case true
spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser Char
escaped = do char '\\' 
             c <- oneOf ['\\', '\"', 'n', 'r', 't'] 
             return $ case c of 
               '\\' -> c
               '"'  -> c
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'


parseString :: Parser Value
parseString = do
                char '"'
                s <- many $ escaped <|> (noneOf ['\"', '\\']) <|> symbol
                char '"'
                return $ String s


--parseAtom :: Parser Value
--parseAtom = many (letter <|> digit <|> symbol) >>= return . Atom
parseAtom :: Parser Value
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

parseChar :: Parser Value
parseChar = do 
			char '\''
			c <- anyChar
			char '\''
			return $ Character c


parseBool :: Parser Value
parseBool = do
        char '#'
        (char 't' >> t) <|> (char 'f' >> f)
  where
    t = return $ Bool True
    f = return $ Bool False

parseFloat :: Parser Value
parseFloat = do 
    integerPart <- many1 digit
    char '.'
    decimalPart <- many1 digit
    let float = read (integerPart ++ ['.'] ++ decimalPart) :: Double
    return $ Float float

parseNumber :: Parser Value
parseNumber = parsePlainNumber <|> parseRadixNumber


parsePlainNumber :: Parser Value
parsePlainNumber = many1 digit >>= return . Number . read


parseRadixNumber :: Parser Value
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )


parseDecimal :: Parser Value
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n


parseBinary :: Parser Value
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n


bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1


readWith f s = fst $ f s !! 0 


parseOctal :: Parser Value
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n


parseHex :: Parser Value
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

parseComplex :: Parser LispVal
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x


parseExpr :: Parser Value
parseExpr = parseAtom
         <|> parseString
         <|> try parseChar
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseNumber 
         <|> parseQuoted
         <|> parseBool
         <|> parseCollection


parseCollection :: Parser Value
parseCollection = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseList :: Parser Value
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser Value
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


parseQuoted :: Parser Value
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


readExpr :: String -> String
readExpr expr = case parsedExpr of
    Left err -> "{no match}: " ++ show err
    Right val -> "{value}: " ++ show val
    where parsedExpr = parse parseExpr "scheme" expr
